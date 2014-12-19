# Spark helper functions

first_last <- function(data) {
    data[c(1, nrow(data)), ]
}


# Base version
# pdf(file = "test-spark-base.pdf", width = 2, height = 1)
# par(mar = c(0, 0, 0, 0))
# with(gen.ref, plot(date, referral.rate, type = "n", ann = F, axes = F))
# with(gen.ref, lines(date, referral.rate, col = portal_colors[8]))
# with(first_last(gen.ref), points(date, referral.rate, col = portal_colors[8], pch = 16))
# dev.off()

# ggplot version
# ggplot(gen.ref, aes(x = date, y= referral.rate)) +
#     geom_line(color = portal_colors[8]) +
#     theme_clean() +
#     geom_point(data = first_last(gen.ref), col = portal_colors[8])
# ggsave(filename = "test-spark-gg.pdf", width = 3, height = 1)


# Gives first and last rows of each data frame
first_last <- function(data) {
    data[c(1, nrow(data)), ]
}



ggplot(gen.ref, aes(x = date, y= referral.rate)) +
    geom_line(color = portal_colors[8]) +
    theme_clean() +
    geom_point(data = first_last(gen.ref), col = portal_colors[8])
ggsave(filename = "test-spark-gg.pdf", width = 3, height = 1)
