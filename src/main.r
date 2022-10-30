# Read data
filePath <- 'data/pill-bug-data-formatted.csv'
file <- read.csv(file = filePath)

transposed <- t(file)

raw_df <- data.frame(file)
colnames(raw_df) <- c('0', '2', '4', '6', '8', '10')

df <- t(raw_df)

colnames(df) <- c('Times',
                '1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10', '11', '12',
                '13', '14')

# Creating analyzed df
a_df <- data.frame(
    Times = c(0, 2, 4, 6, 8, 10),
    Mean=NA,
    SD=NA,
    Mean_Percent = NA,
    SD_Percent = NA
)

number_pill_bugs <- 10

# Computing mean and standard deviation for each time across all classes
for (value in 1:nrow(df)) {
    print(df[value,])
    mean = mean(df[value,])
    sd = sd(df[value,])
    a_df[value, 2] <- mean
    a_df[value, 3] <- sd
    a_df[value, 4] <- (mean/number_pill_bugs)*100
    a_df[value, 5] <- (sd/number_pill_bugs)*100
}

print(a_df)

# Color defs
point_color <- "#64906E"
arrow_color <- "#212B31"
plot_bg <- "#D5DCD1"
bg <- "#f7f7f7"

# Renaming columns
times = t(a_df["Times"])
means = t(a_df["Mean"])
sds = t(a_df["SD"])
means_percent = t(a_df["Mean_Percent"])
sds_percent = t(a_df["SD_Percent"])

y_axis = seq(0, 100, by=20)
y_axis_minor = seq(0, 100, by=5)

# svg("output/plot.svg")
pdf("output/plot.pdf")
# Plotting
par(mar = c(5, 6, 8, 2), lheight=.8, bg=bg)
plot(x=NA,
    xlab=substitute(paste(bold("Time (min)"))), 
    ylab=substitute(paste(bold("Mean Percent of Pill Bugs"))),
    xaxt="none",
    yaxt="none",
    cex.lab=1.5,
    cex.main=1.5,
    ylim=range(c(3.75, 100)),
    xlim=range(c(times - 1, times + 1)),
    pch = 19,
    cex = 3,
    col=point_color,
    )

# Plot background color
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=plot_bg, border=NA)

grid(nx=NA, ny=NULL, col = "gray", lty = 1, lwd = 1.75)

# Creating the legend
legend("topleft", "Moist Filter Paper", fill = point_color, cex = 1.25)

# Drawing axises numbers
axis(1, times, las=0, cex.axis=1.5, font=2)
axis(2, y_axis, las=2, cex.axis=1.5, font=2)

# Drawing error bars
arrows(times, means_percent - sds_percent, times, means_percent + sds_percent, angle = 90, code = 3, length = 0.1, lwd=2, col=arrow_color)

# Drawing point
points(times, means_percent, pch = 19, cex = 3, col=point_color)

# Drawing minor ticks on y-axis
rug(x=y_axis_minor, side=2, ticksize=-0.01)

# Drawing Title
title(main = "Pill Bugs & Filter Paper", cex.main = 2.2, font=2, adj=0, line=4)

# Drawing extra long subtitle
mtext("An Experiment Analyzing The Stimulus \nBehavior Of Pill Bugs Around Moist Filter Paper ", side=3, cex=1.5, font=1, line=.8, adj=0)