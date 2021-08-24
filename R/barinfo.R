library(grid)
library(gridExtra)

#outcomes
n <- 10000
cases_right <- c(600, 200, 100, 100)
cases_left <- c(600, 400, 100, 100)
outcome_texts <- c("Really bad headache", "Headache. Not so bad. But still...", "test 3", "test4")
headline_main_text_left <- "Placebo"
headline_main_text_right <- "Einhornstaub"
headline_1_text_left <- "erhielten"
headline_2_text_left <- "Personen"
headline_2_text_right <- headline_2_text_left


infobar(headline_main_text_left, headline_main_text_right,
        headline_1_text_left, headline_2_text_left,
        headline_2_text_right, n, outcome_texts, cases_right, cases_left,
        outcome_fontface = "plain", big.mark = ".", plot_width = 22,
        bar_size = 2.5, description_size = 4
)

# png(filename="test.png",
#     width=700,
#     height=400,
#     pointsize=12)
#
# infobar(n, cases_right, cases_left, outcome_texts, headline_1_text_left, headline_2_text_left,
#         headline_2_text_right, outcome_fontface = "plain")
#
# dev.off()


#' Title
#'
#' @param n the length of the empty bars
#' @param cases_right
#' @param cases_left
#' @param outcome_texts
#' @param headline_1_text_left
#' @param headline_2_text_left
#' @param headline_2_text_right
#' @param center_distance
#' @param fontsize
#' @param description_size
#' @param bar_size
#' @param n_size
#' @param n_fontface
#' @param col_left
#' @param col_right
#' @param headline2_fontface
#' @param n_height
#' @param headline1_height
#' @param headline2_height
#' @param headline_1_text_right
#' @param headline_size
#' @param distance_box
#' @param box_height
#' @param case_distance
#' @param case_size
#' @param outcome_col
#' @param outcome_size
#' @param outcome_fontface
#' @param outcome_cases_fontface
#' @param headline_main_text_left
#' @param headline_main_text_right
#'
#' @return
#' @export
#'
#' @examples
infobar <- function(headline_main_text_left, headline_main_text_right,
                    headline_1_text_left = "received",
                    headline_2_text_left ="", headline_2_text_right ="",
                    n, outcome_texts, cases_right, cases_left,
  #general settings
  center_distance = 0.01,
  fontsize = 1,
  description_size = 6,
  bar_size = 6,

  # Text sample size
  n_size = 3.5 * fontsize,
  n_fontface = "bold",
  col_left = rgb(0.4, 0.4, 0.4),
  col_right = "#D17A00",

  ###### headlines ######
  headline1_fontface = "plain",
  headline2_fontface = "bold",
  #positioning of the headlines
  n_height = 0.7,
  headline1_height = 0.37,
  headline2_height = 0.2,
  headline_1_text_right = headline_1_text_left,

  headline_size = 1.5 * fontsize,

  ########box_settings#######
  #height of the first box element
  height_first = 0.8,
  #distance from box headline to box
  distance_box = 0.4,
  box_height = 0.4,

  #cases values
  case_distance = 0.01,
  case_size = 1.8 * fontsize,

  # outcomes
  outcome_col = rgb(0.3, 0.3, 0.3),
  outcome_size = 1.5 * fontsize,
  outcome_fontface = "bold",
  outcome_cases_fontface = "bold",

  #number style
  big.mark = ",",
  small.mark = ".",

  plot_width = 16
){

  #creating the grobs

  headline_2_text_right <- paste(formatC(n, format="f", big.mark = big.mark, digits=0), headline_2_text_right)
  headline_2_text_left <- paste(formatC(n, format="f", big.mark = big.mark, digits=0), headline_2_text_left)


  # the text
  n_left <- textGrob(headline_main_text_left, x = 0.5 - center_distance, y = n_height, just="right", gp = gpar(cex=n_size, col = col_left,
                                                                    fontface = n_fontface))
  n_right <- textGrob(headline_main_text_right, 0.5 + center_distance, y = n_height, just="left", gp = gpar(cex=n_size, col = col_right,
                                                                 fontface =n_fontface))
  headline1_left <- textGrob(headline_1_text_left, 0.5 - center_distance, y = headline1_height, just="right",
                        gp = gpar(cex=headline_size, col = col_left, fontface = headline1_fontface))
  headline1_right <- textGrob(headline_1_text_right, 0.5 + center_distance, y = headline1_height, just="left",
                             gp = gpar(cex=headline_size, col = col_right, fontface = headline1_fontface))

  headline2_left <- textGrob(headline_2_text_left, 0.5 - center_distance, y = headline2_height, just="right",
                             gp = gpar(cex=headline_size, col = col_left, fontface = headline2_fontface))
  headline2_right <- textGrob(headline_2_text_right, 0.5 + center_distance, y = headline2_height, just="left",
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
    outcome <- textGrob(outcome_text, 0.5, y = height_first, just="center",
                        gp = gpar(cex=headline_size, col = outcome_col, fontface = outcome_fontface))


    # the bars
    box_left <- rectGrob(x=0.5 - center_distance, y = height_first - distance_box,
                         height = box_height, width=0.45, just="right",
                         gp=gpar(col=col_left))
    box_right <- rectGrob(x=0.5 + center_distance, y = height_first - distance_box,
                          height = box_height, width=0.45, just="left",
                          gp = gpar(col=col_right))


    #width calculation
    w_left <- 0.45 / n * case_left
    w_right <- 0.45 / n * case_right


    #bar filling
    filling_left <- rectGrob(x=0.5 - center_distance, y = height_first - distance_box,
                         height = box_height, width=w_left, just="right",
                         gp=gpar(col=col_left, fill=col_left))
    filling_right <- rectGrob(x=0.5 + center_distance, y = height_first - distance_box,
                          height = box_height, width=w_right, just="left",
                          gp = gpar(col=col_right, fill = col_right))

    #bar text
    if(case_left/n < 0.5){
      value_left <- textGrob(case_left, x= 0.5 - center_distance - w_left - case_distance,
                             y = height_first - distance_box, just="right",
                             gp = gpar(cex=case_size, col = col_left, fontface = outcome_cases_fontface))
    } else {
      value_left <- textGrob(case_left, x= 0.5 - center_distance - w_left + case_distance,
                             y = height_first - distance_box, just="left",
                             gp = gpar(cex=case_size, col = "white", fontface = outcome_cases_fontface))
    }

    if(case_right/n < 0.5){
      value_right <- textGrob(case_right, x= 0.5 + center_distance + w_right + case_distance,
                             y = height_first - distance_box, just="left",
                             gp = gpar(cex=case_size, col = col_right, fontface = outcome_cases_fontface))
    } else {
      value_right <- textGrob(case_right, x= 0.5 + center_distance + w_right - case_distance,
                             y = height_first - distance_box, just="right",
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



