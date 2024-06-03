library(cnasimtools)
library(frscore)
library(data.table)
dsets <- replicate(50, 
                   noisyDat(x = 6, noisefraction = 0.3, add = FALSE), 
                   simplify = FALSE)

targets <- lapply(dsets, function(x) attributes(x)$target)
grab_outcomes <- function(x){
  asfs <- unlist(strsplit(x, "\\)\\*\\("))
  asfs <- sapply(asfs, function(x) gsub("\\(|\\)", "", x), USE.NAMES = F)
  rhss <- sapply(asfs, rhs, USE.NAMES = FALSE)
  return(rhss)
}
outcomes <- lapply(targets, grab_outcomes)
tquant <- 0.98

#fr <- mapply(dsets, function(x) frscored_cna(x, maxsols = 50))
fr <- mapply(\(x,y) frscored_cna(x, maxsols = 200, outcome = y), 
             x = dsets, y = outcomes, SIMPLIFY = FALSE)

fr_r <- lapply(fr, `[[`, 1)
nonnull <- !unlist(lapply(fr_r, is.null))
fr_r <- fr_r[nonnull]
tar_nn <- targets[nonnull]

for (i in seq_along(tar_nn)){
  fr_r[[i]]$correct <- is.submodel(fr_r[[i]]$condition, tar_nn[[i]])
}

fr_r <- lapply(fr_r, \(x) {x$mixscore <- x$norm.score*x$rel.score; x})


fr_top_ns <- lapply(fr_r, 
                 function(x) x[x$norm.score >= quantile(x$norm.score, tquant, na.rm = T),])

fr_top_rel <- lapply(fr_r, 
                    function(x) x[x$rel.score >= quantile(x$rel.score, tquant, na.rm = T),])

fr_top_mix <- lapply(fr_r, 
                     function(x) x[x$mixscore >= quantile(x$mixscore, tquant, na.rm = T),])



all_c <- rbindlist(fr_r, fill = T)
all_c[correct==F, mean(mixscore)]
all_c[, cor(norm.score, correct)]

all_c[correct==T, hist(rel.score, breaks = 40)]

# mp <- glm(correct ~ norm.score, data = indiv_mods_all, family = binomial)
# mpall_norm <- glm(correct ~ norm.score, data = all_c, family = binomial)
# mpall_rel <- glm(correct ~ rel.score, data = all_c, family = binomial)
# avg_comparisons(mpall_norm)
# avg_comparisons(mpall_rel)
# 
# avg_comparisons(mp, comparison = "difference")
# 
# cor(indiv_mods_all$correct, indiv_mods_all$rel.score)

anycorrect_ns <- unlist(lapply(fr_top_ns, function(x) any(x$correct)))
anycorrect_rel <- unlist(lapply(fr_top_rel, function(x) any(x$correct)))
anycorrect_mix <- unlist(lapply(fr_top_mix, function(x) any(x$correct)))
mean(unlist(lapply(fr_top_rel[anycorrect_rel], \(x) max(x$rel.score))))
mean(unlist(lapply(fr_top_rel[!anycorrect_rel], \(x) max(x$rel.score))))
mean(unlist(lapply(fr_top_ns[!anycorrect_ns], \(x) max(x$rel.score))))

length(which(unlist(anycorrect_ns)))
length(which(unlist(anycorrect_rel)))
length(which(unlist(anycorrect_mix)))
# anycor_quant <- rbindlist(anycor_quant)
# anycor_quant[,mean(rel.score)]
# 
# 
# allfalse_quant <- fr_top[!anycorrect]
# allfalse_quant <- rbindlist(allfalse_quant, fill = TRUE)
# allfalse_quant[,mean(rel.score)]
