
library(tsensembler)

all_kernels <- c("rbfdot","vanilladot","polydot","laplacedot")
base_predictors <- c("bm_mars",
                     "bm_ppr",
                     "bm_svr",
                     "bm_glm",
                     "bm_gaussianprocess",
                     "bm_randomforest",
                     "bm_cubist",
                     "bm_pls_pcr",
                     "bm_gbm",
                     "bm_xgb",
                     "bm_ffnn"#,
                     #"bm_timeseries"
)

# test_base_predictors <- c("bm_mars",
#                      "bm_ppr",
#                      "bm_glm",
#                      "bm_randomforest"
#                      #"bm_timeseries"
# )
pars_predictors <- list(bm_gaussianprocess = list(kernel = all_kernels, tol = c(.001,.01)),
                        bm_svr = list(kernel = all_kernels,
                                      epsilon=c(.1,.25,.05),
                                      C=c(1,5,10)),
                        
                        bm_ffnn = list(size = c(5,10,15,50),
                                       decay = c(0.01, 0, .05), maxit=c(100,500, 3000)),
                        bm_mars = list(degree = c(2,5,10), nk = c(7, 15), thresh=c(0.001, 0.01)),
                        bm_glm = list(alpha = c(0,.25,.5,.75,1),
                                      family = c("gaussian")),
                        bm_ppr = list(nterms = c(2,5,7)),
                        bm_gbm = list(interaction.depth = c(1, 3),
                                      dist = c("gaussian"),
                                      shrinkage = c(.01,.001),
                                      n.trees = c(100, 500, 1000)),
                        bm_randomforest = list(num.trees = c(500,250)),
                        bm_pls_pcr = list(method = c("simpls","svdpc")),
                        bm_cubist = list(committees = c(50, 1)))
MODELSPECS <- model_specs(base_predictors,pars_predictors)
#test_specs <- model_specs(test_base_predictors,pars_predictors)
MODELSPECS




library(tsensembler)

all_kernels <- c("rbfdot","vanilladot","polydot","laplacedot")
base_predictors <- c("bm_mars",
                     "bm_ppr",
                     "bm_svr",
                     "bm_glm",
                     #"bm_gaussianprocess",
                     "bm_randomforest",
                     "bm_cubist",
                     "bm_pls_pcr",
                     "bm_gbm",
                     #"bm_xgb",
                     "bm_ffnn"#,
                     #"bm_timeseries"
)

test_base_predictors <- c("bm_mars",
                          "bm_ppr",
                          "bm_glm"
                          #"bm_timeseries"
)
pars_predictors <- list(bm_gaussianprocess = list(kernel = all_kernels),
                        bm_svr = list(kernel = all_kernels),
                        
                        bm_ffnn = list(size = c(7,15),
                                       decay = c(0.01), maxit=c(2000)),
                        bm_mars = list(degree = c(2,5), nk = c(7), thresh=c(0.001)),
                        bm_glm = list(alpha = c(0,.5,1),
                                      family = c("gaussian")),
                        bm_ppr = list(nterms = c(2,5)),
                        #bm_timeseries = list(model = c("bm_tbats")),
                        bm_gbm = list(interaction.depth = c(3),
                                      dist = c("gaussian"),
                                      shrinkage = c(.01),
                                      n.trees = c(500)),
                        bm_randomforest = list(num.trees = c(500,250)),
                        bm_pls_pcr = list(method = c("simpls","svdpc")),
                        bm_cubist = list(committees = c(50, 10, 1)))
MODELSPECS <- model_specs(base_predictors,pars_predictors)
test_specs <- model_specs(test_base_predictors,pars_predictors)
MODELSPECS



