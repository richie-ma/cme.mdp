
#' Reconstruct limit order book(s) from the quote messages extracted from the CME Market by Price data
#'
#'`order_book()` reconstructs limit order book(s) from the quote messages extracted from the CME Market by Price data.
#' This function can process outright limit order book that is reconstructed by all outright quotes, implied order book
#' that is reconstructed by all implied quotes, or consolidated order book that aggregates information of both outright
#' and implied quotes. Outright order book can be supported up to 10 depths, while implied order book can be supported up
#' to 2 depths, according the CME definition.
#'
#' @param mdp_quote_msgs_list A list of quote messages extracted from the MBP data using `quote_messages()`, which must \bold{include} Sunday's file.
#' @param level The maximum book depth that can be supported by the CME, which could be 5 or 10 for futures, 3 for options.
#' When it is not provided, the nearest Sunday's (not a national holiday)
#' MBP/MBO data need to be provided to extract the default depth.
#' @param consolidate If \code{TRUE} (default), return the consolidated order book.
#' @param sunday_input When \code{level} is not provided, the nearest Sunday's (not a national holiday)
#' MBP/MBO data need to be provided to extract the default depth.
#' @param security A symbol of a single contract of a financial security or a vector of multiple single contracts, should be
#' in the format like "CLK9" (crude oil), "ESH9" (E-mini S&P 500), etc. These can
#' be obtained from CME website for a contract specification.
#'
#' @returns A list of outright, implied, and consolidated (if \code{consolidate} = \code{TRUE})
#' limit order books for all tradeable contracts in a trading day stored in a list.
#' @export
#'
#' @import data.table
#'
#' @section Why \code{mdp_quote_msgs_list} needs Sunday's file:
#' One should store all quote messages extracted from the MBP data including Sunday's file
#' in a list because CME disseminates the initial snapshot of the book on Sunday. To ensure the
#' accuracy of book reconstruction, the Sunday's file is needed.
#'
#' @section Processing specific contract(s):
#' In practice, users might not need to process the limit order books of all tradable contracts,
#' which is substantially time-consuming. This function provides a way to only reconstruct the
#' limit order book of a single or multiple trade contracts.
#'
#' @section Limit order book reconstruction:
#' Outright limit order book and implied limit order book are reconstructed separately at first
#' and they should be consolidated finally according to book depths. For the addition update,
#' it indicates a better price than a current book depth enters the market and one should put the previous depths backward,
#' and fill up to ten depths of each side. When a message updated with modification action, one only needs to update the quantity
#' and number of orders with respect to a book depth, while no move occurs. In terms of cancellation,
#' it indicates a current depth will be deleted in a side of the book. It also moves the rest depths forward and fill the first ten depths.
#' Implied limit order book could be constructed with the same logic though the number of orders for each level is not defined in the MBP.
#' The consolidated LOB can be reconstructed according to timestamps or sequence numbers chronologically.
#' If a book depth that exists in the outright book, the quantity of implied book is added to the outright book to reconstruct the
#' consolidated book. When a book depth of implied book is better than the outright book, this depth for the consolidated LOB is the one at implied book
#' and one should move book depths behind this level in the outright LOB backward. Consolidated book is the one that traders monitor during trading.
#'
#' @section Level of limit order book:
#' One should reconstruct the limit order book with the maximum book depth stipulated by the CME to avoid possible errors.
#'
#' @seealso [quote_message()]
#'
#' @examples
#' # This function requires a CME data license to run
#' # Example showing how to reconstruct the limit order book(s)
#' # Know book depth
#' \dontrun{
#' book <- order_book(weekly_msg_quotes_list, "2019-01-07", level = 10)
#'
#' # Unknown book depth
#' book <- order_book(weekly_msg_quotes_list, "2019-01-07", level = 10, sunday_input = sunday_file)
#' }
order_book <- function(mdp_quote_msgs_list,  level=NULL, consolidate=TRUE,  sunday_input=NULL, security = NULL){


  Date <- Code <- Implied <- Symbol  <- MarketDepth <- Seq <- NULL

  ### Order book reconstruction is based on each contract


  if(is.list(mdp_quote_msgs_list)==FALSE){

    stop("Input should be a list that consists all MDP quote messages")

  }

  if(is.null(security)){

    message_all <-  split(rbindlist(lapply(mdp_quote_msgs_list, rbindlist)), by="Code")

  }else{

    message_all <-  split(rbindlist(lapply(mdp_quote_msgs_list, rbindlist)), by="Code")[security]

    if(length(list)==0){
      stop('Cannot find the security in the data input')
    }
  }

    rm(mdp_quote_msgs_list)


    message_all <- lapply(message_all, unique)




    cat("Starting limit order book processing...\n")

    order_book_single <- function(messages, consolidate, level, ...){

    cat("Processing", messages[, unique(Code)], "...\n")

    messages$Update <- as.numeric(messages$Update)
    messages$Seq <- as.numeric(messages$Seq)
    messages$PX <- as.numeric(messages$PX)
    messages$Qty <- as.numeric(messages$Qty)
    messages$Ord <- as.numeric(messages$Ord)
    messages$PX_depth <- as.numeric(messages$PX_depth)
    messages$MsgSeq <- as.numeric(messages$MsgSeq)

    ## implied liquidity messages and outright liquidity messages

    message_outright <- messages[Implied=="N"]

    message_implied <- messages[Implied=="Y"]


    ## outright order book

    if(is.null(level)){

      if(is.null(sunday_input)){

        stop("Sunday input is needed when level is not given")
      }

      definition <- meta_data(sunday_input, date=date)
      level <- definition[Symbol==messages[, unique(Code)], unique(MarketDepth)]
    }





   book <- function(level){

    LOB <- matrix(as.numeric(0),nrow=1, ncol=level*2*3+6)

    book_main_bid <- unlist(mapply(function(x) c(paste0("Bid_PX_", x), paste0("Bid_Qty_", x), paste0("Bid_Ord_", x)), level:1))
    book_main_ask <- unlist(mapply(function(x) c(paste0("Ask_PX_", x), paste0("Ask_Qty_", x), paste0("Ask_Ord_", x)), 1:level))


      colnames(LOB)[c(1:dim(LOB)[2])] <- c("Date", "TransactTime", "Seq", "Code",
                                          book_main_bid, book_main_ask,
                                           "SendingTime", "MsgSeq")

    return(LOB)


}
    ## implied order book always 2 depths


    ######################################## #####################################

    book1 <- function(msg, level, ...){

      book_list <- list()



      for (k in 1:dim(msg)[1]){

        #print(k)

        LOB <- book(level)

        ########################################## Order submission ########################################

        if(msg$Update[k]==0){

          ## bid order
          if(msg$Side[k]==0|msg$Side[k]=="E"){

            column_name1 <- paste0("Bid_PX_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(msg$PX_depth[k]))
            column_index2 <- which(colnames(LOB)==column_name2)
            column_name3 <- paste0("Bid_Ord_",as.character(msg$PX_depth[k]))
            column_index3 <- which(colnames(LOB)==column_name3)

            ## not the last order
            if(column_index1>=5){

              if (k==1){

                LOB[,column_index1] <- msg$PX[k]

                LOB[,column_index2] <- msg$Qty[k]

                LOB[,column_index3] <- msg$Ord[k]

              }
              else{

                LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]

                LOB[,column_index1] <- msg$PX[k]

                LOB[,column_index2] <- msg$Qty[k]

                LOB[,column_index3] <- msg$Ord[k]

                LOB[,c(5:(column_index3-3))] <- book_list[[k-1]][,c(8:column_index3)]

              }
            }


            else {
              LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]

              column_name1 <- paste0("Bid_PX_",as.character(msg$PX_depth[k]))
              column_index1 <- which(colnames(LOB)==column_name1)
              LOB[,column_index1] <- msg$PX[k]

              ## assign Qty
              column_name2 <- paste0("Bid_Qty_",as.character(msg$PX_depth[k]))
              column_index2 <- which(colnames(LOB)==column_name2)
              LOB[,column_index2] <- msg$Qty[k]

              ## assign n_order
              column_name3 <- paste0("Bid_Ord_",as.character(msg$PX_depth[k]))
              column_index3 <- which(colnames(LOB)==column_name3)
              LOB[,column_index3] <- msg$Ord[k]
            }

          }



          ## ask order
          if(msg$Side[k]==1|msg$Side[k]=="F"){

            column_name1 <- paste0("Ask_PX_",as.character(msg$PX_depth[k]))
            column_name2 <- paste0("Ask_Qty_",as.character(msg$PX_depth[k]))
            column_name3 <- paste0("Ask_Ord_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            column_index2 <- which(colnames(LOB)==column_name2)
            column_index3 <- which(colnames(LOB)==column_name3)

            if(column_index1<=(dim(LOB)[2]-2)){

              if(k==1){

                LOB[,column_index1] <- msg$PX[k]

                LOB[,column_index2] <- msg$Qty[k]

                LOB[,column_index3] <- msg$Ord[k]
              }
              else{

                ## process the previous information

                LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]

                LOB[,column_index1] <- msg$PX[k]

                LOB[,column_index2] <- msg$Qty[k]

                LOB[,column_index3] <- msg$Ord[k]

                LOB[,c((column_index1+3):(dim(LOB)[2]-2))] <- book_list[[k-1]][, c(column_index1:(dim(LOB)[2]-5))]

              }

            }
            else {
              ## assign PX
              LOB[, c(5:(dim(LOB)[2]-2))] <-  book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
              column_name1 <- paste0("Ask_PX_",as.character(msg$PX_depth[k]))
              column_index1 <- which(colnames(LOB)==column_name1)
              LOB[,column_index1] <- msg$PX[k]

              ## assign Qty
              column_name2 <- paste0("Ask_Qty_",as.character(msg$PX_depth[k]))
              column_index2 <- which(colnames(LOB)==column_name2)
              LOB[,column_index2] <- msg$Qty[k]

              ## assign n_order
              column_name3 <- paste0("Ask_Ord_",as.character(msg$PX_depth[k]))
              column_index3 <- which(colnames(LOB)==column_name3)
              LOB[,column_index3] <- msg$Ord[k]

            }
          }
        }



        ################################### order modification  ##########################################


        if(msg$Update[k]==1){

          ## bid order

          if(msg$Side[k]==0|msg$Side[k]=="E"){

            if(k!=1){
              LOB[, c(5:(dim(LOB)[2]-2))] <-  book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
            }

            ## assign PX
            column_name1 <- paste0("Bid_PX_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            LOB[,column_index1] <- msg$PX[k]

            ## assign Qty
            column_name2 <- paste0("Bid_Qty_",as.character(msg$PX_depth[k]))
            column_index2 <- which(colnames(LOB)==column_name2)
            LOB[,column_index2] <- msg$Qty[k]

            ## assign n_order
            column_name3 <- paste0("Bid_Ord_",as.character(msg$PX_depth[k]))
            column_index3 <- which(colnames(LOB)==column_name3)
            LOB[,column_index3] <- msg$Ord[k]
          }

          ## ask order
          else {

            if(k!=1){

              LOB[, c(5:(dim(LOB)[2]-2))] <-  book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]

            }
            ## assign PX
            column_name1 <- paste0("Ask_PX_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            LOB[,column_index1] <- msg$PX[k]

            ## assign Qty
            column_name2 <- paste0("Ask_Qty_",as.character(msg$PX_depth[k]))
            column_index2 <- which(colnames(LOB)==column_name2)
            LOB[,column_index2] <- msg$Qty[k]

            ## assign n_order
            column_name3 <- paste0("Ask_Ord_",as.character(msg$PX_depth[k]))
            column_index3 <- which(colnames(LOB)==column_name3)
            LOB[,column_index3] <- msg$Ord[k]

          }
        }


        ################################### order cancellation #####################################################
        if(msg$Update[k]==2){

          ## move forward
          ## bid order
          if(msg$Side[k]==0|msg$Side[k]=="E"){

            column_name1 <- paste0("Bid_PX_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(msg$PX_depth[k]))
            column_index2 <- which(colnames(LOB)==column_name2)
            column_name3 <- paste0("Bid_Ord_",as.character(msg$PX_depth[k]))
            column_index3 <- which(colnames(LOB)==column_name3)

            ## not the last order
            if(k!=1){
              if(column_index3>8){

                LOB[, c(5:(dim(LOB)[2]-2))] <-  book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
                LOB[,c(8:column_index3)] <- book_list[[k-1]][, c(5:(column_index3-3))]
                LOB[, c(5:7)] <- 0

              }

              else {

                LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
                LOB[, c(5:7)] <- 0

              }

            } else{
              LOB[,column_index1] <- 0
              LOB[,column_index2] <- 0
              LOB[,column_index3] <- 0
            }
          }

          ## ask order
          if(msg$Side[k]==1|msg$Side[k]=="F"){

            column_name1 <- paste0("Ask_PX_",as.character(msg$PX_depth[k]))
            column_index1 <- which(colnames(LOB)==column_name1)
            column_name2 <- paste0("Ask_Qty_",as.character(msg$PX_depth[k]))
            column_index2 <- which(colnames(LOB)==column_name2)
            column_name3 <- paste0("Ask_Ord_",as.character(msg$PX_depth[k]))
            column_index3 <- which(colnames(LOB)==column_name3)

            ## not the last order
            if(k!=1){
              if(column_index1<(dim(LOB)[2]-4)){

                LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
                LOB[,c(column_index1:(dim(LOB)[2]-5))] <- book_list[[k-1]][, c((column_index1+3):(dim(LOB)[2]-2))]
                LOB[, c((dim(LOB)[2]-4):(dim(LOB)[2]-2))] <- 0

              }

              else {

                LOB[,c(5:(dim(LOB)[2]-2))] <- book_list[[k-1]][,c(5:(dim(LOB)[2]-2))]
                LOB[, c((dim(LOB)[2]-4):(dim(LOB)[2]-2))] <- 0

              }

            } else{

              LOB[,column_index1] <- 0
              LOB[,column_index2] <- 0
              LOB[,column_index3] <- 0

            }
          }
        }
        book_list[[k]] <- LOB
      }

      LOB <- as.data.table(data.table::transpose(book_list))
     colnames(LOB) <- colnames(book(level))
     LOB$Seq <- msg$Seq



    return(LOB)
    }


  ############################################## outright orders ##############################################

   if(dim(message_outright)[1]!=0){
     cat("Outright limit order book start...\n")
     LOB_outright <- book1(message_outright, level)

   }





    ############################################ implied orders ##############################################
    if(dim(message_implied)[1]!=0){
      cat("Implied limit order book start...\n")
      LOB_implied <- book1(message_implied, 2)
      LOB_implied <- LOB_implied[, -c(7, 10, 13, 16)]

    }

    #rm(message_implied, message_outright)

    ###################################### Consolidated book ###########################################

    if(isTRUE(consolidate)){
      cat("Consolidated limit order book start...\n")

      if((exists("LOB_outright")==TRUE) & (exists("LOB_implied")==FALSE)){

        LOB_conso <- as.data.table(LOB_outright)
        LOB_implied <- NULL
        cat("No implied orders and the consolidated limit order book is the same as the outright limit order book\n")

      }else if((exists("LOB_outright")==FALSE) & (exists("LOB_implied")==TRUE)){

          LOB_conso <- as.data.table(LOB_implied)
          LOB_outright <- NULL
          cat("No outright orders and the consolidated limit order book is the same as the implied limit order book\n")
      }else{

            cat("Both outright order book and implied order book detected\n")

   consolidated_book <- function(LOB_implied, LOB_outright, ...){

     #### constructing the consolidated book

    if(dim(LOB_implied)[1]!=0 & dim(LOB_outright)[1]!=0){

      LOB_implied_new <- matrix(NA,nrow=dim(LOB_outright)[1],ncol=14)

      colnames(LOB_implied_new) <- colnames(book(2))[ -c(7, 10, 13, 16)]

      LOB_implied_new[,3] <- LOB_outright$Seq
      LOB_implied_new <- rbind(LOB_implied_new,LOB_implied)
      LOB_implied_new <- as.data.table(LOB_implied_new)
      setkey(LOB_implied_new,Seq)
      LOB_implied_new[,c(5:12)] <- nafill(LOB_implied_new[,c(5:12)],"locf")
      LOB_implied_new[which(is.na(LOB_implied_new$Bid_PX_1==TRUE)),c(5:12)] <-0

      ###------------------------------------------------------------------------------------------------------------
      LOB_outright_new <- matrix(NA, nrow=dim(LOB_implied)[1], ncol=level*2*3+6)

      colnames(LOB_outright_new) <-  colnames(book(level))

      LOB_outright_new[,3] <- LOB_implied$Seq
      LOB_outright_new <- rbind(LOB_outright_new,LOB_outright)
      LOB_outright_new <- as.data.table(LOB_outright_new)
      setkey(LOB_outright_new,Seq)
      LOB_outright_new[,c(5:64)] <- nafill(LOB_outright_new[,c(5:64)],"locf")
      LOB_outright_new <- as.matrix(LOB_outright_new)

      rm(LOB_implied,LOB_outright)

      ##-----------------------------------------------------------------------------------------------------------------

      #rm(message_implied,message_outright)

      ## consolidating


      LOB_conso_list <- list()

      for (a in 1:dim(messages)[1]) {

        LOB_conso <- book(level)


        #     print(a)

        LOB_conso[, c(5:64)] <- LOB_outright_new[a, c(5:64)]

        if (LOB_implied_new$Bid_PX_1[a]!=0){

          if(LOB_implied_new$Bid_PX_1[a] %in% LOB_conso[,c(seq(5,32,3))]==TRUE){

            bid1_index <- as.numeric(3*which(LOB_conso[,c(seq(5,32,3))]==LOB_implied_new$Bid_PX_1[a], arr.ind = TRUE)+2)

            LOB_conso[,bid1_index] <- LOB_implied_new$Bid_PX_1[a]
            LOB_conso[,bid1_index+1] <- LOB_implied_new$Bid_Qty_1[a]+LOB_conso[,bid1_index+1]


          }

          else{


            px_seq <- as.numeric(LOB_conso[,c(seq(5,32,3))])

            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Bid_PX_1[a])),decreasing = TRUE)==as.numeric(LOB_implied_new$Bid_PX_1[a]))

            if(conso_px <= 10){

              conso_px_lv <- paste0("Bid_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)

              if(conso_px_id >5){


                LOB_conso[, c(5:(conso_px_id-1))] <- LOB_conso[, c(8:(conso_px_id+2))]


                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0

              } else {

                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0

              }



            }
          }
        }

        if(LOB_implied_new$Bid_PX_2[a]!=0 ){



          if(LOB_implied_new$Bid_PX_2[a] %in% LOB_conso[,c(seq(5,32,3))]==TRUE){

            bid2_index <- as.numeric(3*which(LOB_conso[,c(seq(5,32,3))]==LOB_implied_new$Bid_PX_2[a], arr.ind = TRUE)+2)


            LOB_conso[,bid2_index] <- LOB_implied_new$Bid_PX_2[a]
            LOB_conso[,bid2_index+1] <- LOB_implied_new$Bid_Qty_2[a]+LOB_conso[,bid2_index+1]

          }

          else{


            px_seq <- as.numeric(LOB_conso[,c(seq(5,32,3))])

            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Bid_PX_2[a])),decreasing = TRUE)==as.numeric(LOB_implied_new$Bid_PX_2[a]))

            if(conso_px <= 10){

              conso_px_lv <- paste0("Bid_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)

              if(conso_px_id >5){


                LOB_conso[, c(5:(conso_px_id-1))] <- LOB_conso[, c(8:(conso_px_id+2))]

                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0

              } else {

                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0

              }

            }
          }

        }
        ## ask orders
        if (LOB_implied_new$Ask_PX_1[a]!=0 ){

          if(LOB_implied_new$Ask_PX_1[a] %in% LOB_conso[,c(seq(35,64,3))]==TRUE){

            ask1_index <- as.numeric(3*which(LOB_conso[,c(seq(35,64,3))]==LOB_implied_new$Ask_PX_1[a], arr.ind = TRUE)+32)

            LOB_conso[,ask1_index] <- LOB_implied_new$Ask_PX_1[a]
            LOB_conso[,ask1_index+1] <- LOB_implied_new$Ask_Qty_1[a]+LOB_conso[,ask1_index+1]

          }

          else{


            px_seq <- as.numeric(LOB_conso[,c(seq(35,64,3))])

            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Ask_PX_1[a])),decreasing = FALSE)==as.numeric(LOB_implied_new$Ask_PX_1[a]))

            if(conso_px <= 10){

              conso_px_lv <- paste0("Ask_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)

              if(conso_px_id <62){



                LOB_conso[, c((conso_px_id+3):64)] <- LOB_conso[, c(conso_px_id:61)]



                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0

              } else {

                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0

              }

            }
          }
        }

        if(LOB_implied_new$Ask_PX_2[a]!=0 ){

          if(LOB_implied_new$Ask_PX_2[a] %in% LOB_conso[,c(seq(35,64,3))]==TRUE){

            ask2_index <- as.numeric(3*which(LOB_conso[,c(seq(35,64,3))]==LOB_implied_new$Ask_PX_2[a], arr.ind = TRUE)+32)


            LOB_conso[,ask2_index] <- LOB_implied_new$Ask_PX_2[a]
            LOB_conso[,ask2_index+1] <- LOB_implied_new$Ask_Qty_2[a]+LOB_conso[,ask2_index+1]

          }

          else{


            px_seq <- as.numeric(LOB_conso[,c(seq(35,64,3))])

            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Ask_PX_2[a])),decreasing = FALSE)==as.numeric(LOB_implied_new$Ask_PX_2[a]))

            if(conso_px <= 10){

              conso_px_lv <- paste0("Ask_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)

              if(conso_px_id <62){



                LOB_conso[, c((conso_px_id+3):64)] <- LOB_conso[, c(conso_px_id:61)]



                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0

              } else {

                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0

              }

            }
          }


        }

        LOB_conso_list[[a]] <- LOB_conso
      }


      ## assign the date, time, and code

        LOB_conso <- as.data.table(data.table::transpose(LOB_conso_list))
        colnames(LOB_conso) <- colnames(book(level))



      rm(LOB_implied_new,LOB_outright_new)




    }
     LOB_conso$Seq <- messages[, "Seq"]
     LOB_conso$MsgSeq <- messages[, "MsgSeq"]
     LOB_conso$SendingTime <- messages[, "SendingTime"]
     LOB_conso$TransactTime <- messages[, "TransactTime"]
     LOB_conso$Code <- messages[, "Code"]
     setcolorder(LOB_conso, "SendingTime", before = "TransactTime")

    return(LOB_conso)


}


   LOB_conso <- consolidated_book(LOB_implied, LOB_outright)

    }

      }else{

      LOB_conso <- NULL

    }

   if(exists("LOB_outright")){

   LOB_outright$Seq <- message_outright[, "Seq"]
   LOB_outright$MsgSeq <- message_outright[, "MsgSeq"]
   LOB_outright$SendingTime <- message_outright[, "SendingTime"]
   LOB_outright$TransactTime <- message_outright[, "TransactTime"]
   LOB_outright$Code <- message_outright[, "Code"]
   setcolorder(LOB_outright, "SendingTime", before = "TransactTime")

   }

   if(exists("LOB_implied")){
   LOB_implied$Seq <- message_implied[, "Seq"]
   LOB_implied$MsgSeq <- message_implied[, "MsgSeq"]
   LOB_implied$SendingTime <- message_implied[, "SendingTime"]
   LOB_implied$TransactTime <- message_implied[, "TransactTime"]
   LOB_implied$Code <- message_implied[, "Code"]
   setcolorder(LOB_implied, "SendingTime", before = "TransactTime")
   }

   results <- list(LOB_conso=LOB_conso, LOB_outright=LOB_outright, LOB_implied=LOB_implied)


   return(results)
    }

    books<- lapply(message_all, order_book_single, level=level, consolidate=consolidate)
    return(books)
}


