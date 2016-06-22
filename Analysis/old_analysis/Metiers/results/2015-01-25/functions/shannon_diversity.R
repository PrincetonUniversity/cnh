# takes fish tickets and calculates shannon diversity on revenue from metiers

shannon_diversity <- function(x){
  melt_df <- melt(x, id.vars = c("drvid", "metier"), measure.vars = "adj_revenue")
  cast_df <- dcast(melt_df, drvid ~ metier, sum)
  row.names(cast_df) <- cast_df$drvid
  cast_df$drvid <- NULL
  diversity_rev <- diversity(cast_df, index = "shannon")
  return(diversity_rev)
}
