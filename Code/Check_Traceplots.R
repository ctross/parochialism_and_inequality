set.seed(1)

bob = summary(fit[[1]])
GGt_m1 = traceplot(fit[[1]],pars= sample(rownames(bob$c_summary)[c(1:150,54832:54865)],30))
ggsave("Trace[[1]].pdf",GGt_m1,width=11,height=8.5)


set.seed(1)
bob = summary(fit[[2]])
GGt_m1 = traceplot(fit[[2]],pars= sample(rownames(bob$c_summary)[c(1:150,54832:54865)],30))
ggsave("Trace[[2]].pdf",GGt_m1,width=11,height=8.5)

set.seed(1)
bob = summary(fit[[3]])
GGt_m1 = traceplot(fit[[3]],pars= sample(rownames(bob$c_summary)[c(1:150,54832:54865)],30))
ggsave("Trace[[3]].pdf",GGt_m1,width=11,height=8.5)

set.seed(1)
GGt_m1 = traceplot(fit[[4]],pars= sample(rownames(bob$c_summary)[c(1:150,54832:54865)],30))
ggsave("Trace[[4]].pdf",GGt_m1,width=11,height=8.5)
