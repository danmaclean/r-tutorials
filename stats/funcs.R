int_df <- function(n=3){
set.seed("23456")
df1 <- data.frame(compost = rep("john_innes_1", n), supplement = rep("formula1", n), dry_weight = rnorm(n, 45, 10) )
df2 <- data.frame(compost = rep("john_innes_1", n), supplement = rep("formula2", n), dry_weight =  rnorm(n, 30, 8) )

df3 <- data.frame(compost = rep("john_innes_2", n), supplement = rep("formula1",n), dry_weight = rnorm(n, 30, 8))
df4 <- data.frame(compost = rep("john_innes_2", n), supplement = rep("formula2", n), dry_weight = rnorm(n, 45, 8))
df <- dplyr::bind_rows(list(df1,df2,df3,df4))
df$compost <- factor(df$compost, levels = c("john_innes_2", "john_innes_1"))
df$supplement <- factor(df$supplement, levels = c("formula1", "formula2"))
return(df)
}

df <- int_df()
m1 <- lm(dry_weight ~ compost + supplement + compost:supplement, data=df)
summary(m1)
ggplot(df) + aes(compost, dry_weight) + geom_jitter(aes(colour=supplement))
