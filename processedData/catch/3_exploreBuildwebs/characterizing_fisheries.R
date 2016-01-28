# looking at "new" metiers
# eulachon v. smelt, how to deal?
# what about lumping all rockfish together. alluvial plot to see how that changes things?

# characterize by dominant species overall, average landing amount, average revenue amount, number of boats landing (and percent), distribution of "reliance" (percentage of this landed), and number of boats that do this full time. along with percent of landings in each state. time series of number of landings (and amount of landings?). Within group distance?

# useful for discussing results in diversity paper?

met <- "TWL_3"

met_catch <- subset(tickets, metier == met)

met_catch <- met_catch %>%
  group_by(modified) %>%
  summarise(lbs.landed = sum(landed_wt, na.rm = T), 
            dollars.landed = sum(adj_revenue, na.rm =T)) %>%
  arrange(-dollars.landed, -lbs.landed)
ggplot(met_catch[1:30,], aes(x = modified, y = lbs.landed)) + geom_bar(stat='identity') + theme_classic()
ggplot(met_catch[1:30,], aes(x = modified, y = dollars.landed)) + geom_bar(stat='identity') + theme_classic()

# TWL_1: DST
# TWL_2: california halibut
# TWL_3: nearshore draggers? pacific cod/ling cod, widow, chillipepper
# TWL_4: whiting
# TWL_5: baitshrimp
# TWL_6: sea-cucumbers
# TWL_7: yellowtail (whiting bycatch?)

# TLS_1: chinook, pacific halibut
# TLS_2: albacore
# TLS_3: squid
# TLS_4: albacore + chinook (small)
# TLS_5: california halibut (small)
# TLS_6: yellowtail rockfish, lingcod (small)
# TLS_7: white seabass (small)
# TLS_8: lingcod, rockfish (small)
# TLS_9: unspecified rockfish (tiny)
# TLS_10: sablefish, dover, spinyheads
# TLS_11: yellowtail (nano)

# POT_1: dungeness crab
# POT_2: lobster
# POT_3: rock crab
# POT_4: sablefish
# POT_5: spotted prawn
# POT_6: hagfish
# POT_7: diverse: skates, rockcrab, pink shrimp (small): 0 revenue?
# POT_8: other shrimp (small)
# POT_9: sheephead (small)
# POT_10: other crab, other mollusc (small)
# POT_11: diverse rockfish (cabezon, gopher)
# POT_12: other crab, other mollusc (small - reversed majorities from POT_10?)
# POT_13: octopus (small)
# POT_14: eels (small)

# TWS_1: pink shrimp
# TWS_2: ridgeback prawn
# TWS_3: california halibut
# TWS_4: sea cucumbers
# TWS_5: ridgeback prawn, diverse (small)
# TWS_6: unspecified flatfish (small)
# TWS_7: baitshrimp (small)
# TWS_8: other shrimp (small)
# TWS_9: other skate (small)
# TWS_10: other crab, rock crab, small
# TWS_11: white seabass
# TWS_12: vermillion widow (small)

# HKL_1: sablefish, pacific halibut
# HKL_2: black rockfish (plus others)
# HKL_3: california halibut
# HKL_4: diverse rockfish
# HKL_5: lingcod
# HKL_6: diverse rockfish (small)
# HKL_7: white seabass (small)
# HKL_8: vermilion (small)
# HKL_9: sanddabs and scorpion fish (small)
# HKL 10: albacore
# HKL 11: bonito, bigeye, bluefin, and yellow tail tuna (pelagics)
# HKL 12: sheephead, big eye
# HKL 13: thresher, sword fish, yellowtail makos (small)
# HKL 14: smelt
# HKL 15: yellowtail (small)
# HKL 16: surfperch (small)
# HKL 17: mako, widow, bluefin (small)
# HKL 18: unspecified shelf rockfish (small)
# HKl 19: batrays, leopard sharks, other skates (small)
# HKL 20: baracuda
# HKL 21: swordfish
# HKL 22: unspecified red rockfish (small)
# HKL 23: chinook, pacific halibut (small)
# HKL 24: squid, anchovy (small)
# HKL 25: octopus (tiny)
# HKL 26: northern unspecified shelf rockfish (nano)
# HKL 27: angel shark (super nano)

# NET_1: market squid
# NET_2: sardines
# NET_3: california halibut
# NET_4: white seabass
# NET_5: pacific herring and bonito (small)
# NET_6: shad (small)
# NET_7: swordfish (small amounts of thresher, bluefin, mako)
# NET_8: mackeral (small)
# NET_9: anchovy
# NET_10: sockeye
# NET_11: sardine, squid, mackeral (small)
# NET_12: other crab (small)
# NET_13: baracuda (small)
# NET_14: chinook (small)

# MSC_1: red urchin
# MSC_2: razor clam
# MSC_3: ghost shrimp
# MSC_4: sea cucumbers
# MSC_5: cockles
# MSC_6: bait shrimp
# MSC_7: dungeness crab
# MSC_8: swordfish (small)
# MSC_9: unsp mollusks
# MSC_10: other urchins (small)
# MSC_11: gaper clam (small)
# MSC_12: sea cucumbers (small)
# MSC_13: blue mud shrimp (small)
# MSC_14: butter clam (small)
# MSC_15: rock crab (small)
# MSC_16: other mollusk (small)
# MSC_17: rockfish assemblage (small)
# MSC_18: sheephead (small)
# MSC_19: shad (small)
# MSC_20: misc fish, eels (tiny)
# MSC_21: other crab (micro)
# MSC_22: mako (tiny)
# MSC_23: california halibut, unsp flatfish (small)


table(subset(tickets, modified == "SMLT" & year == 2009)$metier)
table(subset(tickets, modified == "EULC" & year == 2010)$metier)
table(subset(tickets, modified == "SMLT" & year == 2010)$metier)
