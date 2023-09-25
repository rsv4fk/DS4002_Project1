coverage_vs_UR_plot <- plot(x=AnalyticFile$coverage_freq, y=AnalyticFile$Unemployment.Rate, main="Level of COVID News Coverage (%) vs. the Unemployment Rate (%)", xlab = "COVID News Coverage (%)", ylab = "Unemployment Rate (%)", xlim = c(0,100), ylim= c(0,100))

plot_reg <- lm(Unemployment.Rate ~ coverage_freq, data = AnalyticFile)
abline(plot_reg, col="blue")

ggplot(AnalyticFile, aes(x=Date)) +
  geom_point(aes(y=coverage_freq, color = "coverage_freq")) + 
  geom_point(aes(y=as.numeric(as.character(Unemployment.Rate)), color="unemp_rate")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("Date (Year-Month)") +
  ylab("COVID News Coverage and Unemployment Rate (%)") +
  ggtitle("Scatterplot of the Level of COVID News Coverage and the Unemployment Rate")
