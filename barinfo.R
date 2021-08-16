library(grid)

box_left <- rectGrob(x=0.45, y=0.5, height = 0.1, width=0.4, just="right")
box_right <- rectGrob(x=0.55, y=0.5, height = 0.1, width=0.4, just="left")


boxes <- gTree(children = gList(box_left, box_right))
grid.draw(boxes)
grid.remove(boxes)
?rectGrob
