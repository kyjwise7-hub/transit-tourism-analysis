# ============================================================
# 지자체 별 대중교통 인프라와 방한 외래관광 활성화 데이터분석
# 강원대학교 정보통계학과 통계 데이터분석 공모전 (2024.11)
# ============================================================


# ── 패키지 로드 ──────────────────────────────────────────────

library(MASS)
library(aplpack)
library(corrplot)
library(ggplot2)


# ── 데이터 불러오기 ──────────────────────────────────────────

travel <- read.csv(file="../data/travel1.csv", header=T, fileEncoding='euc-kr')
summary(travel)
attach(travel)
rownames(travel) <- 광역지자체
travel


# ── 1. 탐색적 데이터 분석 (EDA) ──────────────────────────────

# 방문자수
summary(방문자수)
hist(방문자수, main="방문자수 분포", xlab="방문자수", col="lightblue", border="white")
boxplot(log(방문자수) ~ 광역지자체,
        main="광역지자체별 log(방문자수)",
        las=2, cex.axis=0.6, col="lightblue")
hist(log(방문자수), main="log(방문자수) 분포", xlab="log(방문자수)", col="lightblue", border="white")
qqnorm(log(방문자수), main="QQ plot - log(방문자수)")
qqline(log(방문자수), col="red")
# 로그변환 후 정규분포를 따름

# 소비액
summary(소비액)
hist(소비액, main="소비액 분포", xlab="소비액", col="lightyellow", border="white")
hist(log(소비액), main="log(소비액) 분포", xlab="log(소비액)", col="lightyellow", border="white")
qqnorm(log(소비액), main="QQ plot - log(소비액)")
qqline(log(소비액), col="red")
# 로그변환 후 정규분포에 가까워짐

# 평균숙박기간 (제곱근 변환)
hist(평균숙박기간sqrt, main="평균숙박기간(sqrt) 분포", col="lightgreen", border="white")
hist((평균숙박기간sqrt)^2, main="평균숙박기간 분포", col="lightgreen", border="white")
qqnorm(평균숙박기간sqrt, main="QQ plot - 평균숙박기간sqrt")
qqline(평균숙박기간sqrt, col="red")

# 교통 지표 분포
par(mfrow=c(2,2))
hist(공급도, main="공급도 분포", col="plum", border="white")
hist(log(공급도), main="log(공급도) 분포", col="plum", border="white")
boxplot(공급도, main="boxplot of 공급도", col="plum")
boxplot(log(공급도), main="boxplot of log.공급도", col="plum")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(평균요금, main="평균요금 분포", col="lightyellow", border="white")
hist(접근소요시간_분, main="접근소요시간 분포", col="lightyellow", border="white")
hist(환승대기시간_분, main="환승대기시간 분포", col="lightyellow", border="white")
hist(환승이동시간_분, main="환승이동시간 분포", col="lightyellow", border="white")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
qqnorm(접근소요시간_분, main="QQ - 접근소요시간")
qqline(접근소요시간_분, col="red")
qqnorm(환승대기시간_분, main="QQ - 환승대기시간")
qqline(환승대기시간_분, col="red")
qqnorm(log(환승대기시간_분), main="QQ - log(환승대기시간)")
qqline(log(환승대기시간_분), col="red")
par(mfrow=c(1,1))


# ── 2. 관광 변수 간 관계 탐색 ────────────────────────────────

# 방문자수 vs 소비액
plot(log(방문자수) ~ log(소비액),
     main="log(방문자수) vs log(소비액)",
     pch=19, col="steelblue")
text(y=log(방문자수), x=log(소비액), label=rownames(travel), adj=0.1, cex=0.9)

# 일반 선형 회귀
m0 <- lm(log(방문자수) ~ log(소비액))
plot(log(방문자수) ~ log(소비액), main="일반 선형 회귀", pch=19, col="steelblue")
text(y=log(방문자수), x=log(소비액), label=rownames(travel), adj=0.1, cex=0.9)
abline(m0$coef, lty="dotted", col="red", lwd=2)

# 로버스트 회귀
m1 <- rlm(log(방문자수) ~ log(소비액))
plot(log(방문자수) ~ log(소비액), main="로버스트 회귀", pch=19, col="steelblue")
text(y=log(방문자수), x=log(소비액), label=rownames(travel), adj=0.1, cex=0.9)
abline(m1$coef, lty="dotted", col="darkgreen", lwd=2)

# 커널 밀도 추정
plot(density(log(방문자수)), main="log(방문자수) 커널 밀도")
density1 <- kde2d(log(방문자수), log(소비액), n=25)
image(density1, main="방문자수-소비액 밀도 추정")

# 배그플롯 및 로우에스 곡선
bagplot(x=log(소비액), y=log(방문자수), main="Bagplot - 방문자수 vs 소비액")
s1 <- lowess(log(방문자수) ~ log(소비액), f=1/2)
plot(log(방문자수) ~ log(소비액), main="Lowess 곡선 - 방문자수 vs 소비액", pch=19, col="steelblue")
lines(s1, col="red", lwd=2)

# 방문자수 vs 평균숙박기간
a <- (평균숙박기간sqrt)^2

plot(log(방문자수) ~ a, main="log(방문자수) vs 평균숙박기간", pch=19, col="steelblue")
text(y=log(방문자수), x=a, label=rownames(travel), adj=0.1, cex=0.9)

m2 <- lm(log(방문자수) ~ a)
plot(log(방문자수) ~ a, main="일반 선형 회귀 - 방문자수 vs 숙박기간", pch=19, col="steelblue")
abline(m2$coef, lty="dotted", col="red", lwd=2)

m3 <- rlm(log(방문자수) ~ a)
plot(log(방문자수) ~ a, main="로버스트 회귀 - 방문자수 vs 숙박기간", pch=19, col="steelblue")
abline(m3$coef, lty="dotted", col="darkgreen", lwd=2)

density2 <- kde2d(log(방문자수), a, n=25)
image(density2, main="방문자수-숙박기간 밀도 추정")

bagplot(x=log(방문자수), y=a, main="Bagplot - 방문자수 vs 숙박기간")
s2 <- lowess(log(방문자수) ~ a, f=1)
plot(log(방문자수) ~ a, main="Lowess 곡선 - 방문자수 vs 숙박기간", pch=19, col="steelblue")
lines(s2, col="red", lwd=2)


# ── 3. 상관관계 분석 ─────────────────────────────────────────

travel.b <- data.frame(방문자수, 소비액, 평균숙박기간sqrt, 공급도, 평균요금,
                       접근소요시간_분, 환승이동시간_분, 환승대기시간_분, 수단목적비)
b <- cor(travel.b)
b

corrplot(b, method="color",
         col=colorRampPalette(c("red","white","blue"))(200),
         addCoef.col="black", number.cex=0.7,
         tl.cex=0.8, tl.col="black")


# ── 4. 군집 분석 ─────────────────────────────────────────────

# 엘보우 방법으로 최적 군집 수 탐색
wss <- (nrow(travel.b)-1) * sum(apply(travel.b, 2, var))
for (i in 2:10) wss[i] <- sum(kmeans(travel.b, centers=i)$withinss)
plot(1:10, wss, type="b", pch=19, frame=FALSE,
     main="엘보우 방법 - 최적 군집 수",
     xlab="Number of clusters", ylab="Within-cluster sum of squares",
     col="steelblue")

# 계층적 군집화 (교통 지표 5개)
travel.d <- travel[, c(9:13, 16)]
dist_matrix <- dist(travel.d)
hc <- hclust(dist_matrix, method="complete")

plot(hc, main="Hierarchical Clustering Dendrogram", sub="", xlab="")
rect.hclust(hc, k=4, border="red")
hc1 <- cutree(hc, k=4)
table(hc1)

# 군집 결과 시각화
travel$cluster <- as.factor(hc1)
ggplot(travel, aes(x=log(공급도), y=접근소요시간_분, color=cluster, label=광역지자체)) +
  geom_point(size=3) +
  geom_text(vjust=-0.8, size=3) +
  labs(title="군집 결과 - 공급도 vs 접근소요시간", x="log(공급도)", y="접근소요시간(분)") +
  theme_minimal()


# ── 5. 주성분 분석 (PCA) ─────────────────────────────────────

# 관광 변수 종합지수
travel.a <- data.frame(방문자수, 소비액, 평균숙박기간sqrt)
pca_result <- prcomp(travel.a, scale=TRUE)
summary(pca_result)

weights <- pca_result$rotation[, 1]
index <- 방문자수*weights[1] + 소비액*weights[2] + 평균숙박기간sqrt*weights[3]
index

hist(index, main="관광 종합지수 분포", xlab="종합지수", col="skyblue", border="white")

# 교통 지표 종합지수
travel.e <- data.frame(log(공급도), 평균요금, 접근소요시간_분, 환승대기시간_분, 환승이동시간_분)

pca_result <- prcomp(travel.e, scale=TRUE)
summary(pca_result)

pca_result$rotation
pca_result$sdev^2 / sum(pca_result$sdev^2)
cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))

# 스크리 플롯
plot(pca_result$sdev^2 / sum(pca_result$sdev^2), type="b", pch=19,
     main="스크리 플롯 - 교통 지표 PCA",
     xlab="주성분", ylab="설명된 분산 비율",
     col="steelblue")

# 종합지수 산출 (원점수 × 로딩)
weights <- pca_result$rotation[, 1]
index <- log(공급도)*weights[1] + 평균요금*weights[2] + 접근소요시간_분*weights[3] +
         환승대기시간_분*weights[4] + 환승이동시간_분*weights[5]

travel.e$종합지수 <- index
travel$종합지수 <- travel.e$종합지수
travel.e
travel

# 지자체별 종합지수 막대그래프
ggplot(travel, aes(x=reorder(광역지자체, 종합지수), y=종합지수, fill=종합지수)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient(low="lightblue", high="steelblue") +
  labs(title="지자체별 교통 종합지수", x="광역지자체", y="종합지수") +
  theme_minimal()

# PCA 결과 시각화 (PC1 vs PC2)
pca_df <- data.frame(pca_result$x)
ggplot(pca_df, aes(PC1, PC2)) +
  geom_point(size=3, col="steelblue") +
  geom_text(label=rownames(travel), vjust=-0.8, size=3) +
  labs(title="PCA: PC1 vs PC2", x="Principal Component 1", y="Principal Component 2") +
  theme_minimal()


# ── 6. 다중회귀분석 ──────────────────────────────────────────

M <- lm(travel$종합지수 ~ log(방문자수) + log(소비액) + (평균숙박기간sqrt)^2)
summary(M)

# 회귀 진단 플롯
par(mfrow=c(2,2))
plot(M)
par(mfrow=c(1,1))


# ── 7. 교통 상위/하위 지역 비교 ──────────────────────────────

# 종합지수 순위 기반 상위 3개, 하위 3개 지역 추출
top3_cutoff    <- sort(travel$종합지수, decreasing=TRUE)[3]
bottom3_cutoff <- sort(travel$종합지수)[3]

group_top    <- travel[travel$종합지수 >= top3_cutoff, ]
group_bottom <- travel[travel$종합지수 <= bottom3_cutoff, ]

cat("--- 상위 3개 지역 ---\n")
print(rownames(group_top))
group_top

cat("--- 하위 3개 지역 ---\n")
print(rownames(group_bottom))
group_bottom

# 기술통계 비교
cat("--- 상위 지역 기술통계 ---\n")
summary(group_top[, c("방문자수", "소비액", "평균숙박기간sqrt")])
cat("--- 하위 지역 기술통계 ---\n")
summary(group_bottom[, c("방문자수", "소비액", "평균숙박기간sqrt")])

# 윌콕슨 검정 (n=3 소표본, 참고용)
result <- wilcox.test(group_top$방문자수, group_bottom$방문자수)
print(result)

result <- wilcox.test(group_top$소비액, group_bottom$소비액)
print(result)

result <- wilcox.test(group_top$평균숙박기간sqrt, group_bottom$평균숙박기간sqrt)
print(result)

# 상위/하위 지역 비교 시각화
group_top$그룹    <- "상위"
group_bottom$그룹 <- "하위"
group_compare <- rbind(group_top, group_bottom)

ggplot(group_compare, aes(x=그룹, y=방문자수, fill=그룹)) +
  geom_boxplot(alpha=0.7) +
  geom_jitter(width=0.1, size=2) +
  labs(title="상위/하위 지역 방문자수 비교", x="그룹", y="방문자수") +
  theme_minimal()

ggplot(group_compare, aes(x=그룹, y=소비액, fill=그룹)) +
  geom_boxplot(alpha=0.7) +
  geom_jitter(width=0.1, size=2) +
  labs(title="상위/하위 지역 소비액 비교", x="그룹", y="소비액") +
  theme_minimal()


# ── 8. 스타 플롯 ─────────────────────────────────────────────

x <- travel[, 9:13]
stars(x, scale=T, col.stars=1:17, segments=T,
      main="지자체별 교통 지표 스타 플롯")


# ============================================================
# 공모전 이후 개선 코드
# ============================================================


# ── 개선 1. 순열 검정 (Wilcoxon 소표본 한계 대안) ────────────

# install.packages("coin")
library(coin)

방문자수_group <- c(group_top$방문자수, group_bottom$방문자수)
label <- factor(c(rep("top", nrow(group_top)), rep("bottom", nrow(group_bottom))))

oneway_test(방문자수_group ~ label, distribution="exact")

# 그룹 기준 확장 (상위/하위 5개 지역)
top5_cutoff    <- sort(travel$종합지수, decreasing=TRUE)[5]
bottom5_cutoff <- sort(travel$종합지수)[5]

group_top5    <- travel[travel$종합지수 >= top5_cutoff, ]
group_bottom5 <- travel[travel$종합지수 <= bottom5_cutoff, ]

result <- wilcox.test(group_top5$방문자수, group_bottom5$방문자수, exact=FALSE)
print(result)

result <- wilcox.test(group_top5$소비액, group_bottom5$소비액, exact=FALSE)
print(result)

# 기술통계 기반 직접 비교
desc_top <- data.frame(
  변수 = c("방문자수", "소비액", "평균숙박기간"),
  상위평균 = c(mean(group_top$방문자수),
               mean(group_top$소비액),
               mean((group_top$평균숙박기간sqrt)^2)),
  하위평균 = c(mean(group_bottom$방문자수),
               mean(group_bottom$소비액),
               mean((group_bottom$평균숙박기간sqrt)^2))
)
desc_top$차이 <- desc_top$상위평균 - desc_top$하위평균
print(desc_top)


# ── 개선 2. PCA 해석 개선 (로딩 방향 확인 + 표준화 점수) ─────

travel.e <- data.frame(log(공급도), 평균요금, 접근소요시간_분,
                       환승대기시간_분, 환승이동시간_분)
data_scaled <- scale(travel.e)

pca_result <- prcomp(data_scaled, scale.=TRUE)

# 각 주성분의 설명 분산 비율 확인
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var <- cumsum(var_explained)

cat("각 주성분 설명 분산 비율:\n")
print(round(var_explained, 3))
cat("누적 설명 분산 비율:\n")
print(round(cum_var, 3))
# PC1의 설명력이 60% 이상이면 단독 사용 가능
# 그렇지 않으면 PC2까지 포함 검토

# 로딩값 부호 및 방향 확인
cat("\nPC1 로딩값:\n")
print(pca_result$rotation[, 1])
# 접근소요시간, 환승대기시간 등은 클수록 불리한 변수
# 로딩 부호가 양수라면 PC1이 높을수록 교통이 불편한 방향을 의미
# 필요시 부호 반전 후 사용: pca_result$x[,1] * -1

# 표준화된 주성분 점수로 종합지수 계산 (권장)
travel$종합지수_개선 <- pca_result$x[, 1]

# PC1 설명력 부족 시 PC1+PC2 가중 합산
w1 <- var_explained[1]
w2 <- var_explained[2]
travel$종합지수_2pc <- (pca_result$x[,1] * w1 + pca_result$x[,2] * w2) / (w1 + w2)

# 바이플롯으로 변수-주성분 관계 시각화 (ggplot2)
library(ggrepel)

scores   <- as.data.frame(pca_result$x[, 1:2])
scores$지자체 <- rownames(travel)
scale_factor <- max(abs(scores[,1:2])) / max(abs(pca_result$rotation[,1:2])) * 0.55
loadings <- as.data.frame(pca_result$rotation[, 1:2] * scale_factor)
loadings$변수 <- c("log(공급도)", "평균요금", "접근소요시간", "환승대기시간", "환승이동시간")
var_exp  <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)

ggplot() +
  annotate("path",
           x = cos(seq(0, 2*pi, length.out=200)) * max(abs(scores[,1:2])) * 0.9,
           y = sin(seq(0, 2*pi, length.out=200)) * max(abs(scores[,1:2])) * 0.9,
           color="gray88", linewidth=0.4, linetype="dashed") +
  geom_hline(yintercept=0, color="gray80", linewidth=0.4) +
  geom_vline(xintercept=0, color="gray80", linewidth=0.4) +
  geom_segment(data=loadings,
               aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow=arrow(length=unit(0.22,"cm"), type="closed"),
               color="#E53935", linewidth=0.75) +
  geom_label(data=loadings,
             aes(x=PC1*1.18, y=PC2*1.18, label=변수),
             color="#B71C1C", fill="white", size=3.1,
             linewidth=0.2, label.padding=unit(0.15,"lines")) +
  geom_point(data=scores,
             aes(x=PC1, y=PC2),
             color="#1565C0", size=2.4, alpha=0.85) +
  geom_text_repel(data=scores,
                  aes(x=PC1, y=PC2, label=지자체),
                  color="#1A237E", size=2.9,
                  max.overlaps=20,
                  box.padding=0.4, point.padding=0.2,
                  segment.color="gray70", segment.size=0.3) +
  labs(title="PCA Biplot — 교통 지표",
       x=paste0("PC1  (", var_exp[1], "%)"),
       y=paste0("PC2  (", var_exp[2], "%)"),
       caption="빨간 화살표: 교통 지표 로딩 방향  |  파란 점: 광역지자체") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=13),
        plot.caption=element_text(color="gray55", size=8, hjust=0.5),
        panel.grid=element_blank(),
        axis.title=element_text(color="gray40"))
