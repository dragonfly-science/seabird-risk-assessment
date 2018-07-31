library(data.table)
library(ggplot2)

if (file.exists('../risk-ratios.rds')) {
    
    riskratios <- readRDS('../risk-ratios.rds')
    setnames(riskratios, 'species', 'code')
    spplist <- fread('../../../input-data/species-list.csv')
    riskratios[spplist, species := upper1st(i.common_name), on = 'code']
    riskratios[, species := factor(species, levels = sort(unique(species), decreasing=T))]

    col <- "#4292C6"
    ggplot(riskratios, aes(x = species, y = riskratio)) +
        geom_violin(scale = 'width', fill = alpha(col, 0.7), colour = col) +
        coord_flip(ylim = c(0, 100)) +
        scale_y_continuous(trans = 'log1p', breaks = c(0, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000)) +
        labs(y = 'Risk ratio (= APF / PST)', x = NULL) +
        geom_hline(yintercept = 1) +
        theme_light() +
        theme(panel.grid.minor=element_blank(),
              axis.text.y = element_text(size = 10))
    ggsave('assets/risk-ratios.png', width = 8, height = 2)

} else {
    cat('No risk calculated. Skip.\n')
    if (file.exists('assets/risk-ratios.png')) {
        file.remove('assets/risk-ratios-%s.png')
    }
}
