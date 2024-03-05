library(haven)
# merge all ifls
load(file="~/iflseast.Rdata")
iflseast <- comb_2
iflseast <- iflseast %>% mutate(int.id=paste0(int.id,"_e"))
load(file="~/ifls4.Rdata")
ifls4 <- comb_2 %>% select(!health_wght_w_attr)
ifls4 <- ifls4 %>% mutate(int.id=paste0(int.id,"_4"))
load(file="~/ifls5.Rdata")
ifls5 <- comb_2 %>% select(!health_wght_w_attr) %>% select(!year)
ifls5 <- ifls5 %>% mutate(int.id=paste0(int.id,"_5"))

comb_2 <- rbind(iflseast,ifls4,ifls5)


ll_P <- length(levels(comb_2$province))
levels(comb_2$province) <- paste0(rep("P",ll_P),1:ll_P)

ll_MCP <- length(levels(comb_2$municipality))
levels(comb_2$municipality) <- paste0(rep("MCP",ll_MCP),1:ll_MCP)

ll_SBD <- length(levels(comb_2$subdistrict))
levels(comb_2$subdistrict) <- paste0(rep("SBD",ll_SBD),1:ll_SBD)

comb_2$int.id <- factor(comb_2$int.id)
ll_INT <- length(levels(comb_2$int.id))
levels(comb_2$int.id) <- paste0(rep("int",ll_INT),1:ll_INT)

save(comb_2, file="~/iflsmerged.Rdata")
