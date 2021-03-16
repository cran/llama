fold = data.frame(a=rep.int(0, 10), b=c(rep.int(1, 5), rep.int(0, 5)),
                  c=c(rep.int(0, 5), rep.int(1, 5)))
d = list(data=rbind(cbind(fold, id=1:10), cbind(fold, id=11:20)),
         train=list(1:nrow(fold)),
         test=list(1:nrow(fold) + nrow(fold)),
         features=c("a"),
         ids=c("id"),
         minimize=T,
         performance=c("b", "c"),
         best=rep.int(c(rep.int("c", 5), rep.int("b", 5)), 2))
class(d) = "llama.data"
attr(d, "hasSplits") = TRUE

fold.algo = data.frame(a=rep.int(0, 20), p=c(rep.int(c(1, 0), 5), rep.int(c(0, 1), 5)),
                       f=rep.int(c(2, 3), 10), algo=rep(c("b", "c"), 10))
d.algo = list(data=rbind(cbind(fold.algo, id=rep.int(1:10, rep.int(2, 10))), cbind(fold.algo, id=rep.int(11:20, rep.int(2, 10)))),
              train=list(1:nrow(fold.algo)),
              test=list(1:nrow(fold.algo) + nrow(fold.algo)),
              features=c("a"),
              algorithmFeatures=c("f"),
              ids=c("id"),
              algos=c("algo"),
              minimize=T,
              performance=c("p"),
              algorithmNames=c("b", "c"),
              best=rep.int(c(rep.int("c", 5), rep.int("b", 5)), 2))
class(d.algo) = "llama.data"
attr(d.algo, "hasSplits") = TRUE


fold.three = data.frame(a=rep.int(0, 10), b=c(rep.int(1, 5), rep.int(0, 5)),
                  c=c(rep.int(0, 5), rep.int(1, 5)), d = rep.int(1, 10))
d.three = list(data=rbind(cbind(fold.three, id=1:10), cbind(fold.three, id=11:20)),
         train=list(1:nrow(fold.three)),
         test=list(1:nrow(fold.three) + nrow(fold.three)),
         features=c("a"),
         ids=c("id"),
         minimize=T,
         performance=c("b", "c", "d"),
         best=rep.int(c(rep.int("c", 5), rep.int("b", 5)), 2))
class(d.three) = "llama.data"
attr(d.three, "hasSplits") = TRUE


fold.three.algo = data.frame(a=rep.int(0, 30), p=c(rep.int(c(1, 0, 1), 5), rep.int(c(0, 1, 1), 5)),
                       f=rep.int(c(2, 3, 4), 10), algo=rep(c("b", "c", "d"), 10))
d.three.algo = list(data=rbind(cbind(fold.three.algo, id=rep.int(1:10, rep.int(3, 10))), cbind(fold.three.algo, id=rep.int(11:20, rep.int(3, 10)))),
               train=list(1:nrow(fold.three.algo)),
               test=list(1:nrow(fold.three.algo) + nrow(fold.three.algo)),
               features=c("a"),
               algorithmFeatures=c("f"),
               ids=c("id"),
               algos=c("algo"),
               minimize=T,
               performance=c("p"),
               algorithmNames=c("b", "c", "d"),
               best=rep.int(c(rep.int("c", 5), rep.int("b", 5)), 2))
class(d.three.algo) = "llama.data"
attr(d.three.algo, "hasSplits") = TRUE


folde = data.frame(a=c(rep.int(0, 5), rep.int(1, 5)),
                  b=c(rep.int(1, 5), rep.int(0, 5)),
                  c=rep.int(1, 10))
e = list(data=rbind(cbind(folde, id=1:10), cbind(folde, id=11:20)),
         train=list(1:nrow(folde)),
         test=list(1:nrow(folde) + nrow(folde)),
         features=c("c"), minimize=T,
         performance=c("a", "b"),
         ids=c("id"),
         best=rep.int("b", 20))
class(e) = "llama.data"
attr(e, "hasSplits") = TRUE


foldf = data.frame(a=rep.int(0, 10), b=c(rep.int(1, 5), rep.int(0, 5)),
                  c=c(rep.int(0, 5), rep.int(1, 5)))
dnosplit = list(data=rbind(cbind(foldf, id=1:10), cbind(foldf, id=11:20)),
         features=c("a"),
         ids=c("id"),
         minimize=T,
         performance=c("b", "c"),
         best=rep.int(c(rep.int("c", 5), rep.int("b", 5)), 2))
class(dnosplit) = "llama.data"


foldg = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), c=rep.int(0, 10),
                   d=rep.int(T, 10), e=rep.int(F, 10))
g = list(data=rbind(cbind(foldg, id=1:10), cbind(foldg, id=11:20)),
         train=list(1:nrow(foldg)),
         test=list(1:nrow(foldg) + nrow(foldg)),
         features=c("a"),
         performance=c("b", "c"),
         success=c("d", "e"),
         ids=c("id"),
         minimize=T,
         best=rep.int("b", 20))
class(g) = "llama.data"
attr(g, "hasSplits") = TRUE

bests = c("a", "a", "a", "b", "b")
bestlist = list("a", "b", c("a", "b"))
bestlistlong = list("a", "a", "b", c("a", "b"), "a", "a", c("a", "b"), "a", "a", "a")

foldmeas = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
dmeas = list(data=rbind(cbind(foldmeas, id=1:5), cbind(foldmeas, id=6:10)),
    test=list(1:5, 6:10), performance=c("a", "b"), minimize=T, ids=c("id"),
    success=c("d", "e"))

foldmeas.algo = data.frame(p=rep.int(c(1, 0), 5),
                      s=rep.int(c(F, T), 5), a=rep(c("a", "b"), 5), f=rep.int(1, 5))
dmeas.algo = list(data=rbind(cbind(foldmeas.algo, id=rep.int(1:5, rep.int(2, 5))), cbind(foldmeas.algo, id=rep.int(6:10, rep.int(2, 5)))),
             test=list(1:10, 11:20), performance=c("p"), minimize=T, ids=c("id"), 
             success=c("s"), algos=("a"), algorithmFeatures=c("f"), algorithmNames=c("a", "b"))

asmeas = data.frame(algorithm=rep.int("a", 5), score=1, iteration=1)
bsmeas = data.frame(algorithm=rep.int("b", 5), score=1, iteration=1)
asmeas.algo = data.frame(algorithm=rep.int("a", 10), score=1, iteration=1)
bsmeas.algo = data.frame(algorithm=rep.int("b", 10), score=1, iteration=1)

modelameas = list(predictions=rbind(cbind(asmeas, id=1:5), cbind(asmeas, id=6:10)))
class(modelameas) = "llama.model"
attr(modelameas, "hasPredictions") = TRUE
modelbmeas = list(predictions=rbind(cbind(bsmeas, id=1:5), cbind(bsmeas, id=6:10)))
class(modelbmeas) = "llama.model"
attr(modelbmeas, "hasPredictions") = TRUE

modelameas.algo = list(predictions=rbind(cbind(asmeas.algo, id=rep.int(1:5, rep.int(2, 5))), cbind(asmeas.algo, id=rep.int(6:10, rep.int(2, 5)))))
class(modelameas.algo) = "llama.model"
attr(modelameas.algo, "hasPredictions") = TRUE
modelbmeas.algo = list(predictions=rbind(cbind(bsmeas.algo, id=rep.int(1:5, rep.int(2, 5))), cbind(bsmeas.algo, id=rep.int(6:10, rep.int(2, 5)))))
class(modelbmeas.algo) = "llama.model"
attr(modelbmeas.algo, "hasPredictions") = TRUE

foldone = data.frame(a=rep.int(1, 10), b=rep.int(2, 10), c=rep.int(1.5, 10), d=rep.int(1, 10))
one = list(data=rbind(cbind(foldone, id=1:10), cbind(foldone, id=11:20)),
         train=list(1:nrow(foldone)),
         test=list(1:nrow(foldone) + nrow(foldone)),
         features=c("d"), minimize=T,
         performance=c("a", "b", "c"),
         ids=c("id"),
         best=rep.int("a", 20))
class(one) = "llama.data"
attr(one, "hasSplits") = TRUE


