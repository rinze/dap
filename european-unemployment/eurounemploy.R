library(ggplot2)
theme_set(theme_bw(20))
rm(list = ls())
euro <- read.csv("data/une_rt_m_1_Data.csv", stringsAsFactors = FALSE)

# Let's clean the data
euro$Value <- as.numeric(euro$Value)
euro$Flag.and.Footnotes <- NULL
euro$AGE <- factor(euro$AGE)
euro$GEO <- factor(euro$GEO)
euro$S_ADJ <- factor(euro$S_ADJ)
euro$SEX <- factor(euro$SEX)

# Correct Germany's name here
l1 <- levels(euro$GEO)
l1[grepl("Germany", l1)] <- "Germany"
levels(euro$GEO) <- l1

# Get true date, and month and year from euro$TIME variable
tmp1 <- strsplit(euro$TIME, "M")
tmp1 <- do.call(rbind, tmp1)
euro$year <- as.numeric(tmp1[, 1])
euro$month <- as.numeric(tmp1[, 2])
euro$date <- as.Date(sprintf("%s-%02d-01", euro$year, euro$month))

# Get only countries we want (remove all average European metrics)
euro <- euro[!grepl("Euro", euro$GEO), ]
euro$GEO <- factor(euro$GEO)

# Only use seasonally adjusted data
euro <- euro[euro$S_ADJ == "Seasonally adjusted data", ]
euro$S_ADJ <- NULL

avg_unemployment <- aggregate(Value ~ year + SEX + GEO + AGE, euro, mean, 
                              na.rm = TRUE)
# And now get the minimum (and the maximum for later). The minimum is taken before
# the crisis
min_unemployment <- aggregate(Value ~ SEX + GEO + AGE, 
                              avg_unemployment[avg_unemployment$year < 2007, ], 
                              min, 
                              na.rm = TRUE)
# The maximum is taken after the crisis
max_unemployment <- aggregate(Value ~ SEX + GEO + AGE, 
                              avg_unemployment[avg_unemployment$year >= 2007, ], 
                              max, 
                              na.rm = TRUE)
names(min_unemployment) <- c("SEX", "GEO", "AGE", "min_unemployment")
names(max_unemployment) <- c("SEX", "GEO", "AGE", "max_unemployment")

# Normalize
euro <- merge(euro, min_unemployment)
euro <- merge(euro, max_unemployment)
euro$norm_unemployment <- euro$Value / euro$min_unemployment

# Compute the ratio between the minimum and the maximum
euro$ratio <- with(euro, max_unemployment / min_unemployment)
euro$GEO <- reorder(euro$GEO, -euro$max_unemployment, min)

euro$group <- paste(euro$SEX, euro$AGE)

euro_total <- euro[euro$SEX == "Total" & euro$AGE == "Total", ]
euro_partial <- euro[euro$SEX != "Total" & euro$AGE != "Total", ]

plt1 <- ggplot(euro_partial) + 
    geom_line(data = euro_total, aes(x = date, y = Value),
              color = "black", alpha = 0.2, size = 2) +
    geom_line(aes(x = date,
                  y = Value, 
                  linetype = AGE,
                  color = SEX,
                  group = group), size = 1) +
    facet_wrap(~ GEO, ncol = 5, scales = "free_y") +
    scale_linetype_manual(values = c(2, 3), name = "Age") +
    scale_color_manual(values = c("red", "blue"), name = "Sex") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Unemployment rate [%]\n") + xlab("\nDate") +
    ggtitle("Unemployment rates in Europe, 2002 - 2015\nGray line represents total average\n")

print(plt1)
ggsave(plt1, filename = "/tmp/mean_unemployment.pdf", height = 20, width = 25)

# Order according to the ratio
euro$GEO <- reorder(euro$GEO, -euro$ratio, min)

euro$group <- paste(euro$SEX, euro$AGE)
euro_total <- euro[euro$SEX == "Total" & euro$AGE == "Total", ]
euro_partial <- euro[euro$SEX != "Total" & euro$AGE != "Total", ]

plt2 <- ggplot(euro_partial) + 
    geom_line(data = euro_total, aes(x = date, y = norm_unemployment),
              color = "black", alpha = 0.2, size = 2) +
    geom_line(aes(x = date,
                  y = norm_unemployment, 
                  linetype = AGE,
                  color = SEX,
                  group = group), size = 1) +
    facet_wrap(~ GEO, ncol = 5, scales = "free_y") +
    scale_linetype_manual(values = c(2, 3), name = "Age") +
    scale_color_manual(values = c("red", "blue"), name = "Sex") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Increase in unemployment rates from minimum point before 2007\n") + 
    xlab("\nDate") +
    ggtitle("Increase in unemployment rates in Europe, 2002 - 2015, from minimum levels before 2006\nGray line represents total average\n")

print(plt2)
ggsave(plt2, filename = "/tmp/mean_norm_unemployment.pdf", height = 20, width = 25)

