# ============================================================
# README 시각화 자료 생성 스크립트
# outputs/figures/ 에 PNG 3개를 저장합니다
# ============================================================

library(ggplot2)
library(corrplot)

# ── 데이터 로드 ──────────────────────────────────────────────

travel <- read.csv(file="../data/travel1.csv", header=T, fileEncoding='euc-kr')
attach(travel)
rownames(travel) <- 광역지자체

# ── 교통 지표 PCA 및 종합지수 산출 ───────────────────────────

travel.e <- data.frame(log(공급도), 평균요금, 접근소요시간_분,
                       환승대기시간_분, 환승이동시간_분)
pca_result <- prcomp(travel.e, scale=TRUE)

weights <- pca_result$rotation[, 1]
travel$종합지수 <- log(공급도)*weights[1] + 평균요금*weights[2] +
                   접근소요시간_분*weights[3] + 환승대기시간_분*weights[4] +
                   환승이동시간_분*weights[5]

# ── Figure 1: 지자체별 교통 종합지수 ─────────────────────────

png("../outputs/figures/01_transport_index.png",
    width=800, height=560, res=110)

ggplot(travel, aes(x=reorder(광역지자체, 종합지수),
                   y=종합지수, fill=종합지수)) +
  geom_bar(stat="identity", width=0.7) +
  coord_flip() +
  scale_fill_gradient(low="#90CAF9", high="#1565C0") +
  labs(title="지자체별 대중교통 종합지수 (PCA PC1)",
       x=NULL, y="종합지수",
       caption="교통 지표 5개(공급도·요금·접근·환승대기·환승이동)를 PCA로 축약") +
  theme_minimal(base_size=11) +
  theme(legend.position="none",
        plot.title=element_text(face="bold", hjust=0.5),
        plot.caption=element_text(color="gray50", size=8))

dev.off()
cat("Figure 1 saved.\n")

# ── Figure 2: 상관관계 히트맵 ────────────────────────────────

travel.b <- data.frame(
  방문자수, 소비액, 평균숙박기간sqrt,
  공급도, 평균요금, 접근소요시간_분,
  환승이동시간_분, 환승대기시간_분
)
b <- cor(travel.b)

png("../outputs/figures/02_correlation_heatmap.png",
    width=760, height=680, res=110)

corrplot(b,
         method="color",
         col=colorRampPalette(c("#D32F2F","white","#1565C0"))(200),
         addCoef.col="black", number.cex=0.65,
         tl.cex=0.82, tl.col="black",
         title="관광 변수 × 교통 지표 상관관계",
         mar=c(0,0,2,0))

dev.off()
cat("Figure 2 saved.\n")

# ── Figure 3: PCA 바이플롯 ───────────────────────────────────

png("../outputs/figures/03_pca_biplot.png",
    width=760, height=680, res=110)

biplot(pca_result,
       main="PCA Biplot — 교통 지표",
       cex=c(0.8, 0.9),
       col=c("steelblue", "tomato"),
       xlabs=rownames(travel))

dev.off()
cat("Figure 3 saved.\n")
cat("모든 시각화 파일이 outputs/figures/ 에 저장되었습니다.\n")
