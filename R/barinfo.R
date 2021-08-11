library(grid)

center_distance <- 0.02

# Text sample size
n <- 1000
n_size = 4
n_fontface = "bold"
col_left = rgb(0.3, 0.3, 0.3)
col_right = "#D17A00"


# headlines

headline_fontface = "plain"
headline_1_text_left <- "Treated Patients"
headline_1_text_right <- headline_1_text_left

headline_2_text_left <- "Boring Mushrooms"
headline_2_text_right <- "Exciting Mushrooms"

headline_size = 1.5

########box_settings#######
height_first <- 0.53
distance_box <- 0.1

# outcomes
outcome_text <- "Really bad headache"
outcome_col <- rgb(0.3, 0.3, 0.3)
outcome_size <- 1.5
outcome_fontface <- "bold"


# the text
n_left <- textGrob(n, x = 0.5 - center_distance, y = 0.8, just="right", gp = gpar(cex=n_size, col = col_left,
                                                                  fontface = n_fontface))
n_right <- textGrob(n, 0.5 + center_distance, y = 0.8, just="left", gp = gpar(cex=n_size, col = col_right,
                                                               fontface =n_fontface))
headline1_left <- textGrob(headline_1_text_left, 0.5 - center_distance, y = 0.7, just="right",
                      gp = gpar(cex=headline_size, col = col_left, fontface = headline_fontface))
headline1_right <- textGrob(headline_1_text_right, 0.5 + center_distance, y = 0.7, just="left",
                           gp = gpar(cex=headline_size, col = col_right, fontface = headline_fontface))

headline2_left <- textGrob(headline_2_text_left, 0.5 - center_distance, y = 0.7 -  headline_size/25, just="right",
                           gp = gpar(cex=headline_size, col = col_left, fontface = headline_fontface))
headline2_right <- textGrob(headline_2_text_right, 0.5 + center_distance, y = 0.7 - headline_size/25, just="left",
                            gp = gpar(cex=headline_size, col = col_right, fontface = headline_fontface))

infotext <- gTree(children = gList(n_left, n_right, headline1_left, headline1_right,
                                   headline2_left, headline2_right))

# now starting with the individual outcomes
outcome <- textGrob(outcome_text, 0.5, y = height_first, just="center",
                    gp = gpar(cex=headline_size, col = outcome_col, fontface = outcome_fontface))


# the bars
grid.draw(rectGrob(gp=gpar(lty=0)))
box_left <- rectGrob(x=0.5 - center_distance, y = height_first - distance_box,
                     height = 0.1, width=0.45, just="right",
                     gp=gpar(col=col_left))
box_right <- rectGrob(x=0.5 + center_distance, y = height_first - distance_box,
                      height = 0.1, width=0.45, just="left",
                      gp = gpar(col=col_right))

boxes <- gTree(children = gList(outcome, box_left, box_right))

grid.draw(boxes)
grid.draw(infotext)

sample_vp <- viewport(x = 0, y = 0,
                      width = 1, height = 1,
                      just = c("left", "bottom"))
pushViewport(sample_vp)


