
#' Visualization of limit order book
#'
#' `book_bar()` plots the limit order book with \code{ggplot2::geom_bar()} and an
#' animated plot is also supported.
#'
#' @param mbp_order_book Limit order book data generated via `order_book()`,
#' can be either outright, implied, or consolidated books.
#' @param implied_book Implied limit order book generated via `order_book()`
#' to show the implied liquidity proportion in consolidated book. Default is \code{NULL}
#' and no implied order book is visualized.
#' @param level Number of books depths are visualized. Default is \code{NULL} and
#' all book depths are visualized.
#' @param animation Plot the animated limit order book. Default is \code{FALSE}, and
#' static plots are returned.
#' @param anim_fps Frame per second (FPS) for the animated plot to control the play
#' speed. Default is \code{10}.
#'
#' @returns A list of ggplot2 (static) limit order book plot and an animated limit order book
#' plot (if \code{animation = TRUE}).
#' @export
#'
#' @import data.table ggplot2 ggnewscale stringr
#'
#' @section Data recommendation:
#' One would better use resampled data instead of tick-level data to reduce the load
#' in plotting. Resampling can be done via `cme_resample()`.
#'
#' @section Animated plot:
#' Limit order book data should have multiple timestamps to process the animated plot.
#'
#' @seealso [cme_resample()]
#'
#' @examples
#' # This function requires a CME data license to run
#' # Static plot
#' \dontrun{
#' static_plot <- book_bar(book, level = 10, animation = FALSE)
#'
#' # Animated plot
#' animation <- book_bar(book, level = 10, animation = TRUE, anim_fps = 40)
#' }
 book_bar <- function(mbp_order_book, implied_book=NULL, level=NULL, animation=FALSE, anim_fps=10){


   Date <- Time <- Seq  <- Code   <-    Bid_PX_10 <-  Bid_Qty_10 <- Bid_Ord_10 <-  Bid_PX_9 <-  Bid_Qty_9 <- Bid_Ord_9  <- Bid_PX_8  <-
   Bid_Qty_8  <- Bid_Ord_8  <- Bid_PX_7  <-  Bid_Qty_7  <- Bid_Ord_7  <-  Bid_PX_6  <-  Bid_Qty_6  <- Bid_Ord_6  <- Bid_PX_5  <-  Bid_Qty_5  <- Bid_Ord_5  <-
   Bid_PX_4  <-  Bid_Qty_4  <- Bid_Ord_4  <- id_PX_3  <-  Bid_Qty_3  <- Bid_Ord_3  <- Bid_PX_2  <-  Bid_Qty_2   <-Bid_Ord_2   <-Bid_PX_1   <- Bid_Qty_1  <-
   Bid_Ord_1  <- Ask_PX_1  <-  Ask_Qty_1  <- Ask_Ord_1  <- Ask_PX_2  <-  Ask_Qty_2  <- Ask_Ord_2   <- Ask_PX_3   <- Ask_Qty_3   <-Ask_Ord_3  <- Ask_PX_4   <-
   Ask_Qty_4  <- Ask_Ord_4  <- Ask_PX_5  <-  Ask_Qty_5  <- Ask_Ord_5  <- Ask_PX_6   <- Ask_Qty_6  <-  Ask_Ord_6  <- Ask_PX_7  <-  Ask_Qty_7  <- Ask_Ord_7  <-
   Ask_PX_8   <- Ask_Qty_8  <- Ask_Ord_8  <- Ask_PX_9  <-  Ask_Qty_9   <-Ask_Ord_9 <-  Ask_PX_10  <- Ask_Qty_10  <-Ask_Ord_10  <- MsgSeq   <- NULL

   fields <- side <- depth <- values <- px <- Q <- O <- cum_Q <- type <- anim_book_plot <-
     transition_time <- view_follow<- ease_aes <- animate <- NULL


   if (is.data.table(mbp_order_book)){
     if("DT" %in% colnames(mbp_order_book) == TRUE){

       setnames(mbp_order_book, "DT", "Time")

     } else {
       stop("Input has to be data.table")
     }
   }


   if(is.null(level)==F){

     columns <- c(unlist(mapply(function(x) c(paste0("Bid_PX_", x), paste0("Bid_Qty_", x), paste0("Bid_Ord_", x)), level:1)),
                  unlist(mapply(function(x) c(paste0("Ask_PX_", x), paste0("Ask_Qty_", x), paste0("Ask_Ord_", x)), 1:level)))

     columns_delete <- colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)][which(colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)] %in% columns ==F)]

     if(length(columns_delete)!=0){

     mbp_order_book <- mbp_order_book[, !columns_delete, with = FALSE]

     }
   }

  dcast_data <- function(data, ...){

  melted_data <- melt(data, c("Time", "Code"), measure.vars =patterns("PX_|Qty_|Ord_"),
               variable.name= "fields",
               value.name = c("values"))[, `:=`(fields=substr(fields, 5, 5), side=substr(fields, 1, 3),
                                                depth=as.numeric(fifelse(str_sub(fields, -1)=="0", "10", str_sub(fields, -1))))][, px:=values[fields=="P"], by=c("side", "depth", "Time")][! fields=="P"]

  decasted_data <- dcast(melted_data, Time + Code + px + side + depth ~ fields, value.var = c("values"))


  setkey(decasted_data, side, depth)

  return(decasted_data)
  }

  mbp_order_book <- dcast_data(mbp_order_book)



  if(is.null(implied_book)){

    if(isTRUE(animation) & mbp_order_book[, length(unique(Time))]==1){

      stop("Animated figure needs dataset at multiple timestamps")

    }

    if(isTRUE(animation)==FALSE & mbp_order_book[, length(unique(Time))]!=1){

      stop("Static figure only needs dataset at a single timestamp")
    }

    book_plot <- ggplot()+
      geom_col(data = mbp_order_book[side=="Ask"], aes(x = px, y = Q, fill = O),
               alpha=0.7) +
      geom_text(data = mbp_order_book[side == "Ask" & depth!=1], aes(x = px, y = Q, label = px),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask"& depth!=1], aes(x = px, y = Q, label = Q),
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask"& depth==1], aes(x = px, y = Q, label = Q, fontface = "bold"),
                hjust=1, position = position_fill(vjust = 0))+
      scale_fill_gradientn(
        name = "#Ord: Ask",
        colors = c("green1", "yellow"),  ### Ask
        limits = c(mbp_order_book[side == "Ask", min(O)], mbp_order_book[side == "Ask", max(O)]),
        guide = guide_legend(order = 1, reverse = TRUE)
      )+

      new_scale_fill() +
      geom_col(data = mbp_order_book[side=="Bid"], aes(x = px, y = -Q, fill = O),
               alpha=0.7) +
      geom_text(data = mbp_order_book[side == "Bid" & depth!=1], aes(x = px, y = -Q, label = px),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid" & depth==1], aes(x = px, y = -Q, label = px, fontface = "bold"),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid"& depth!=1], aes(x = px, y = -Q, label = Q),
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid"& depth==1], aes(x = px, y = -Q, label = Q, fontface = "bold"),
                hjust=1, position = position_fill(vjust = 0))+
      geom_hline(yintercept = 0, linetype = 1, linewidth=0.5, color = "blue")+
      scale_y_continuous(labels = abs)+
      scale_fill_gradientn(
        name = "#Ord: Bid",
        colors = c("red1", "yellow"),  ###Bid
        limits = c(mbp_order_book[side == "Bid", min(O)], mbp_order_book[side == "Bid", max(O)]),
        guide = guide_legend(order = 2, reverse = TRUE)
      )+
      labs(title = mbp_order_book[, Time], ## bbo info
           x = "Price",
           y = "Quantity") +
      coord_flip()+
      theme_minimal()


  }else{

    if(dim(mbp_order_book)!=dim(implied_book)){

      stop("Outright book and implied book do not have the same number of observations. Resampling may be needed")
    }



    if("DT" %in% colnames(implied_book) == T){

      setnames(implied_book, "DT", "Time")

    }

    if(isTRUE(animation) & implied_book[, length(unique(Time))]==1){

      stop("Animated figure needs dataset at multiple timestamps")

    }

    if(isTRUE(animation)==FALSE & implied_book[, length(unique(Time))]!=1){

      stop("Static figure only needs dataset at a single timestamp")
    }




    implied_book <- dcast_data(implied_book)
    implied_book[, type:="Implied"]
    mbp_order_book[, type:="Outright"]
    merged <- rbind(mbp_order_book, implied_book, fill=TRUE)

    merged[, cum_Q:=sum(Q), by=c("Time", "Code", "px")]


    book_plot <- ggplot()+
      geom_col(data = merged[side=="Ask"], aes(x = px, y = Q, fill = type),
               alpha=0.7) +
      scale_fill_manual(name = "Ord Type", values = c("Implied"="grey"),
                        guide = guide_legend(order = 1, reverse = TRUE))+

      new_scale_fill()+

      geom_col(data = mbp_order_book[side=="Ask"], aes(x = px, y = Q, fill = O),
               alpha=0.7)+

      geom_text(data = mbp_order_book[side == "Ask" & depth!=1], aes(x = px, y = Q, label = px),
                hjust=0, position = position_fill(vjust=0))+
      geom_text(data = mbp_order_book[side == "Ask" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),
                hjust=0, position = position_fill(vjust=0))+
      geom_text(data = merged[side == "Ask"& depth!=1 & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q),
                hjust=1, position = position_fill(vjust=0))+
      geom_text(data = merged[side == "Ask"& depth==1  & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q, fontface = "bold"),
                hjust=1, position = position_fill(vjust=0))+
      scale_fill_gradientn(
        name = "#Ord: Ask",
        colors = c("green1", "yellow"),  ### Ask
        limits = c(mbp_order_book[side == "Ask", min(O)], mbp_order_book[side == "Ask", max(O)]),
        guide = guide_legend(order = 2, reverse = TRUE)
      )+

      new_scale_fill() +
      geom_col(data = merged[side=="Bid"], aes(x = px, y = Q, fill = type),
               alpha=0.7) +
      scale_fill_manual(name = "Ord Type", values = c("Implied"="grey"),
                        guide = guide_legend(order = 1, reverse = TRUE))+

      new_scale_fill()+

      geom_col(data = mbp_order_book[side=="Bid"], aes(x = px, y = -Q, fill = O),
               alpha=0.7) +

      geom_text(data = mbp_order_book[side == "Bid" & depth!=1], aes(x = px, y = -Q, label = px),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid" & depth==1], aes(x = px, y = -Q, label = px, fontface = "bold"),
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = merged[side == "Bid"& depth==1 & type=="Outright"], aes(x = px, y = -cum_Q, label = cum_Q, fontface = "bold"),
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = merged[side == "Bid"& depth!=1 & type=="Outright"], aes(x = px, y = -cum_Q, label = cum_Q),
                hjust=1, position = position_fill(vjust = 0))+
      geom_hline(yintercept = 0, linetype = 1, linewidth=0.5, color = "blue")+
      scale_y_continuous(labels = abs)+
      scale_fill_gradientn(
        name = "#Ord: Bid",
        colors = c("red1", "yellow"),  ###Bid
        limits = c(mbp_order_book[side == "Bid", min(O)], mbp_order_book[side == "Bid", max(O)]),
        guide = guide_legend(order = 3, reverse = TRUE)
      )+
      labs(title = mbp_order_book[, Time], ## bbo info
           x = "Price",
           y = "Quantity") +
      coord_flip()+
      theme_minimal()
  }





  if(isTRUE(animation)){

    stopifnot("package:gganimate" %in% search() || requireNamespace("gganimate", quietly = TRUE))


    anim_book_plot <- function(data,...){

      book_plot <- book_plot+
        labs(title = paste0(data[,unique(Code)],"  ", 'Time: {frame_time}'), ## bbo info
             x = "Price",
             y = "Quantity") +
        transition_time(Time) +
        view_follow()+
        ease_aes('linear')

      book_plot <- animate(book_plot, nframes=data[, length(unique(Time))], fps=anim_fps, height=800, width = 1200)
      return(book_plot)
    }

    if(is.null(implied_book)){

      book_plot <- anim_book_plot(mbp_order_book)

    }else{

      book_plot <- anim_book_plot(merged)
    }



  }
  return(book_plot)

 }


