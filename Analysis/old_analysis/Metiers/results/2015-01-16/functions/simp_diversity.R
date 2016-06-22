# takes a fish-ticket data frame with metiers present and returns the simpson diversity of each drvid based on adjusted revenue

simp_diversity <- function(x){
  melt_df <- melt(x, id.vars = c("drvid", "metier"), measure.vars = "adj_revenue")
  cast_df <- dcast(melt_df, drvid ~ metier, sum)
  row.names(cast_df) <- cast_df$drvid
  cast_df$drvid <- NULL
  diversity_rev <- diversity(cast_df, index = "simpson")
  return(diversity_rev)
}

