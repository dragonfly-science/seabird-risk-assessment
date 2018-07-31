MODELS = stan
MODELPATHS = $(addprefix model/,$(MODELS))
MODELREPORTS = $(addsuffix /model-report.html,$(MODELPATHS))
DATE := $(shell date +%Y-%m-%d) 

RUN ?= docker run -i --rm --net=host --user=$$(id -u):$$(id -g) -v$$(pwd):/work -w /work --env RUN= dragonflyscience/seabird-risk-assessment:2017-10-18

all: $(MODELREPORTS)


derived-data/gridded-distribution-by-species.csv: \
			input-data/species-distributions/Diomedea_exulans.shp \
			input-data/species-distributions/Diomedea_epomophora.shp \
			make-gridded-distribution.r
	$(RUN) bash -c "Rscript make-gridded-distribution.r"

### <! MAPS !> ###
maps/.species_maps: derived-data/gridded-distribution-by-species.csv \
		maps/species-maps.r
	$(RUN) bash -c "cd maps && Rscript species-maps.r && touch .species_maps"

maps/.effort_maps: input-data/fishing-effort-observed.csv \
		input-data/fishing-effort-total.csv \
		maps/effort-maps.r
	$(RUN) bash -c "cd maps && Rscript effort-maps.r && touch .effort_maps"



### <! PREPARE !> ###
prep: model/.prepared

model/.prepared: 	parameters.r \
			input-data/southern-hemisphere-5-degree.shp \
			input-data/fishing-effort-total.csv \
			input-data/fishing-effort-observed.csv \
			derived-data/gridded-distribution-by-species.csv \
			input-data/species-list.csv \
			input-data/observed-captures.csv \
			input-data/demographic-parameters-processed.csv \
			model/prepare-model-data.r
	$(RUN) bash -c "cd model  &&  Rscript prepare-model-data.r && touch .prepared"


### <! MODELS !> ###
model: %/.risk

# .PRECIOUS: %/model-results.rdata
%/model-results.rdata: model/.prepared  %/model.*  %/run-model.r
	$(RUN) bash -c "cd $* && Rscript run-model.r"

# .PRECIOUS: %/mcmc-results.rdata
%/mcmc-results.rdata: %/model-results.rdata %/get-results.r
	$(RUN) bash -c "cd $* && Rscript get-results.r"

%/.risk: %/mcmc-results.rdata %/calc-pst-and-risk.r
	$(RUN) bash -c "cd $* && Rscript calc-pst-and-risk.r && touch .risk"

### <! REPORTING !> ###a
%/model-report.html: model/model-report.Rmd \
		%/.observed-vs-predicted.r \
	 	%/.apf-summaries.r \
		%/.apf-maps.r \
		%/.diagnostics.r \
		%/.risk-ratios.r \
		maps/.species_maps \
		maps/.effort_maps
	$(RUN) bash -c "cd $* && cp ../model-report.Rmd . && Rscript -e 'rmarkdown::render(\"model-report.Rmd\", clean=F)' && rm model-report.Rmd"

%/.apf-summaries.r: %/.risk model/apf-summaries.r
	$(RUN) bash -c "cd $* && mkdir -p assets && Rscript ../apf-summaries.r && touch .apf-summaries.r"

%/.observed-vs-predicted.r: %/.risk model/observed-vs-predicted.r
	$(RUN) bash -c "cd $* && mkdir -p assets && Rscript ../observed-vs-predicted.r  &&  touch .observed-vs-predicted.r"

%/.apf-maps.r: %/.risk model/apf-maps.r
	$(RUN) bash -c "cd $* && mkdir -p assets && Rscript ../apf-maps.r && touch .apf-maps.r"

%/.diagnostics.r: %/.risk model/diagnostics.r
	$(RUN) bash -c "cd $* && mkdir -p assets && Rscript ../diagnostics.r  &&  touch .diagnostics.r"

%/.risk-ratios.r: %/.risk model/risk-ratios.r
	$(RUN) bash -c "cd $* && mkdir -p assets && Rscript ../risk-ratios.r  &&  touch .risk-ratios.r"

### <! Docker image !> ###
.docker: Dockerfile
	docker build -t dragonflyscience/seabird-risk-assessment .  && \
	docker tag dragonflyscience/seabird-risk-assessment dragonflyscience/seabird-risk-assessment:$(DATE) && \
	docker tag dragonflyscience/seabird-risk-assessment dragonflyscience/seabird-risk-assessment:latest && \
	touch .docker

docker-push: .docker
	docker push dragonflyscience/seabird-risk-assessment:$(DATE) && \
	docker push dragonflyscience/seabird-risk-assessment:latest

### <! Utils !> ###
clean:
	$(RUN) rm -fr model/data.rdata model/modeldata.rdata
	for modelpath in $(MODELPATHS) ; do \
		$(RUN) rm -fr $$modelpath/model-results.rdata; \
		$(RUN) rm -fr $$modelpath/mcmc-results.rdata; \
		$(RUN) rm -fr $$modelpath/mcmc-results-summary.rds; \
		$(RUN) rm -fr $$modelpath/pst-samples.rds; \
		$(RUN) rm -fr $$modelpath/pst-samples-summary.csv; \
		$(RUN) rm -fr $$modelpath/risk-ratios.rds; \
		$(RUN) rm -fr $$modelpath/risk-ratios-summary.csv; \
		$(RUN) rm -fr $$modelpath/diag.txt; \
	done

test:
	@echo $(MODELPATHS)
	@echo $(MODELS)
	@echo $(RUN)
