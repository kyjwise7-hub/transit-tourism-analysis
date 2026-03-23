# ============================================================
# README 시각화 자료 생성 스크립트
# outputs/figures/ 에 PNG 3개를 저장합니다
# ============================================================

library(ggplot2)
library(corrplot)
library(ggrepel)

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

# ── Figure 3: PCA 바이플롯 (ggplot2) ────────────────────────

# 관측치 좌표 (지자체)
scores <- as.data.frame(pca_result$x[, 1:2])
scores$지자체 <- rownames(travel)

# 변수 로딩 (화살표) — 관측치 스케일에 맞게 조정
scale_factor <- max(abs(scores[,1:2])) / max(abs(pca_result$rotation[,1:2])) * 0.55
loadings <- as.data.frame(pca_result$rotation[, 1:2] * scale_factor)
loadings$변수 <- c("log(공급도)", "평균요금", "접근소요시간", "환승대기시간", "환승이동시간")

# 설명 분산 비율
var_exp <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)

png("../outputs/figures/03_pca_biplot.png",
    width=820, height=720, res=120)

ggplot() +
  # 배경 원
  annotate("path",
           x = cos(seq(0, 2*pi, length.out=200)) * max(abs(scores[,1:2])) * 0.9,
           y = sin(seq(0, 2*pi, length.out=200)) * max(abs(scores[,1:2])) * 0.9,
           color="gray88", linewidth=0.4, linetype="dashed") +
  # 십자선
  geom_hline(yintercept=0, color="gray80", linewidth=0.4) +
  geom_vline(xintercept=0, color="gray80", linewidth=0.4) +
  # 변수 화살표
  geom_segment(data=loadings,
               aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow=arrow(length=unit(0.22,"cm"), type="closed"),
               color="#E53935", linewidth=0.75) +
  # 변수 레이블
  geom_label(data=loadings,
             aes(x=PC1*1.18, y=PC2*1.18, label=변수),
             color="#B71C1C", fill="white", size=3.1,
             label.size=0.2, label.padding=unit(0.15,"lines")) +
  # 지자체 점
  geom_point(data=scores,
             aes(x=PC1, y=PC2),
             color="#1565C0", size=2.4, alpha=0.85) +
  # 지자체 레이블 (겹침 방지)
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

dev.off()
cat("Figure 3 saved.\n")
cat("모든 시각화 파일이 outputs/figures/ 에 저장되었습니다.\n")
