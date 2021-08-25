#' Create infoplot with bars
#'
#' @import grid gridExtra
#'
#' @param n the length of the empty bars
#' @param cases_right number of cases right
#' @param cases_left number of cases left
#' @param outcome_texts the headlines of the outcomes (describing the invidual bars)
#' @param headline_main_text_left main headline text left
#' @param headline_main_text_right main headline text right
#' @param headline_1_text_left top subheadline text left
#' @param headline_1_text_right top subheadline text right
#' @param headline_2_text_left bottom subheadline text left
#' @param headline_2_text_right bottom subheadline text right
#' @param center_distance vertical white space size
#' @param fontsize overall fontsize
#' @param description_size space used for the main and subheadline
#' @param bar_size height of bar size and associated outcome texts
#' @param n_size overall sample size
#' @param headline_main_fontface either "plain", "bold", "italic", or "bold.italic"
#' @param col_left colour left column
#' @param col_right colour right column
#' @param headline2_fontface either "plain", "bold", "italic", or "bold.italic"
#' @param headline_main_height main headline height
#' @param headline1_height top subheadline space height
#' @param headline2_height bottom subheadline space height
#' @param headline_size subheadline fontsize
#' @param distance_bar distance outcome text to bars. Values between 0.4 to 0.6 looks best in most cases
#' @param bar_height height of the bars
#' @param case_distance distance "n cases" text to bar
#' @param case_size size of "n cases" text
#' @param outcome_col outcome text color
#' @param outcome_size font size outcomes
#' @param outcome_fontface either "plain", "bold", "italic", or "bold.italic"
#' @param outcome_cases_fontface either "plain", "bold", "italic", or "bold.italic"
#' @param big.mark symbol used for big marks (thousands)
#' @param decimal.mark decimal symbol
#' @param plot_width plot width
#'
#' @return
#' @export
#'
#' @examples
infobar <- function(headline_main_text_left, headline_main_text_right,
                    headline_1_text_left = "received",
                    headline_1_text_right = "received",
                    headline_2_text_left ="", headline_2_text_right ="",
                    n, outcome_texts, cases_right, cases_left,
  #general settings
  center_distance = 0.01,
  fontsize = 1,
  description_size = 4,
  bar_size = 2.5,

  # Text sample size
  n_size = 3.5 * fontsize,
  headline_main_fontface = "bold",
  col_left = rgb(0.4, 0.4, 0.4),
  col_right = "#D17A00",

  ###### headlines ######
  headline1_fontface = "plain",
  headline2_fontface = "bold",
  #positioning of the headlines
  headline_main_height = 0.7,
  headline1_height = 0.37,
  headline2_height = 0.2,

  headline_size = 1.5 * fontsize,

  ########box_settings#######
  #height of the first box element
  height_first = 0.8,
  #distance from box headline to box
  distance_bar = 0.4,
  bar_height = 0.4,

  #cases values
  case_distance = 1,
  case_size = 1.8,

  # outcomes
  outcome_col = rgb(0.3, 0.3, 0.3),
  outcome_size = 1.5,
  outcome_fontface = "bold",
  outcome_cases_fontface = "bold",

  #number style
  big.mark = ",",
  decimal.mark = ".",

  plot_width = 22
){

  case_distance <- case_distance / 100
  case_size = case_size * fontsize
  outcome_size = outcome_size * fontsize


  #creating the grobs

  headline_2_text_right <- paste(formatC(n, format="f", big.mark = big.mark, decimal.mark = decimal.mark, digits=0), headline_2_text_right)
  headline_2_text_left <- paste(formatC(n, format="f", big.mark = big.mark, decimal.mark = decimal.mark, digits=0), headline_2_text_left)


  # the text
  n_left <- grid::textGrob(headline_main_text_left, x = 0.5 - center_distance, y = headline_main_height, just="right", gp = gpar(cex=n_size, col = col_left,
                                                                    fontface = headline_main_fontface))
  n_right <- grid::textGrob(headline_main_text_right, 0.5 + center_distance, y = headline_main_height, just="left", gp = gpar(cex=n_size, col = col_right,
                                                                 fontface =headline_main_fontface))
  headline1_left <- grid::textGrob(headline_1_text_left, 0.5 - center_distance, y = headline1_height, just="right",
                        gp = gpar(cex=headline_size, col = col_left, fontface = headline1_fontface))
  headline1_right <- grid::textGrob(headline_1_text_right, 0.5 + center_distance, y = headline1_height, just="left",
                             gp = gpar(cex=headline_size, col = col_right, fontface = headline1_fontface))

  headline2_left <- grid::textGrob(headline_2_text_left, 0.5 - center_distance, y = headline2_height, just="right",
                             gp = gpar(cex=headline_size, col = col_left, fontface = headline2_fontface))
  headline2_right <- grid::textGrob(headline_2_text_right, 0.5 + center_distance, y = headline2_height, just="left",
                              gp = gpar(cex=headline_size, col = col_right, fontface = headline2_fontface))

  infotext <- gTree(children = gList(n_left, n_right, headline1_left, headline1_right,
                                     headline2_left, headline2_right))

  grobs <- list(infotext)

  for(i in 1:length(cases_left)){
      case_right <- cases_right[i]
      case_left <- cases_left[i]
      outcome_text <- outcome_texts[i]


    # now starting with the individual outcomes
    #outcome name
    outcome <- grid::textGrob(outcome_text, 0.5, y = height_first, just="center",
                        gp = gpar(cex=outcome_size, col = outcome_col, fontface = outcome_fontface))


    # the bars
    box_left <- rectGrob(x=0.5 - center_distance, y = height_first - distance_bar,
                         height = bar_height, width=0.45, just="right",
                         gp=gpar(col=col_left))
    box_right <- rectGrob(x=0.5 + center_distance, y = height_first - distance_bar,
                          height = bar_height, width=0.45, just="left",
                          gp = gpar(col=col_right))


    #width calculation
    w_left <- 0.45 / n * case_left
    w_right <- 0.45 / n * case_right


    #bar filling
    filling_left <- rectGrob(x=0.5 - center_distance, y = height_first - distance_bar,
                         height = bar_height, width=w_left, just="right",
                         gp=gpar(col=col_left, fill=col_left))
    filling_right <- rectGrob(x=0.5 + center_distance, y = height_first - distance_bar,
                          height = bar_height, width=w_right, just="left",
                          gp = gpar(col=col_right, fill = col_right))

    #bar text
    if(case_left/n < 0.5){
      value_left <- grid::textGrob(case_left, x= 0.5 - center_distance - w_left - case_distance,
                             y = height_first - distance_bar, just="right",
                             gp = gpar(cex=case_size, col = col_left, fontface = outcome_cases_fontface))
    } else {
      value_left <- grid::textGrob(case_left, x= 0.5 - center_distance - w_left + case_distance,
                             y = height_first - distance_bar, just="left",
                             gp = gpar(cex=case_size, col = "white", fontface = outcome_cases_fontface))
    }

    if(case_right/n < 0.5){
      value_right <- grid::textGrob(case_right, x= 0.5 + center_distance + w_right + case_distance,
                             y = height_first - distance_bar, just="left",
                             gp = gpar(cex=case_size, col = col_right, fontface = outcome_cases_fontface))
    } else {
      value_right <- grid::textGrob(case_right, x= 0.5 + center_distance + w_right - case_distance,
                             y = height_first - distance_bar, just="right",
                             gp = gpar(cex=case_size, col = "white", fontface = outcome_cases_fontface))
    }

    grobs[[i+1]] <- gTree(children = gList(outcome, box_left, box_right, filling_left, filling_right,
                                    value_left, value_right))
  }



  grid.newpage()


  heights <- rep(unit(bar_size, "cm"), length(grobs))
  heights[1] <- unit(description_size, "cm")

  grid.arrange(grobs=grobs, heights=heights, ncol=1, widths = unit(plot_width, "cm"))

}



