source("scripts/00_config.R")
check <- function(ok,msg) if(!isTRUE(ok))stop(msg)
expect_error <- function(expr)check(inherits(try(force(expr),silent=TRUE),"try-error"),"Expected error")
set.seed(72)
n<-1200L
d<-data.frame(game_id=as.character(rep(1:20,each=n/20)),play_id=as.character(seq_len(n)),game_index=rep(1:20,each=n/20),week=rep(c(rep(1:15,length.out=17),16:18),each=n/20),kickoff_utc=rep(sprintf("2021-09-%02dT12:00:00Z",1:20),each=n/20),rusher_id=sample(as.character(101:112),n,TRUE),double_team=rep(c(0,1,0),length.out=n),double_team_unknown=rep(c(0,0,1),length.out=n),complete_timing=rep(c(1,1,0),length.out=n),win_target=sample(0:1,n,TRUE),severity_outcome=sample(model_config()$classes,n,TRUE),sack_credit=0)
d$early_model_eligible<-1L
d$strict_recorded_pressure<-as.integer(d$severity_outcome=="pressure")
d$win_target[d$severity_outcome=="win"]<-1
d$win_target[d$severity_outcome=="loss"]<-0
d$rusher_name<-paste0("Rusher ",d$rusher_id)
d$sack_credit[d$severity_outcome=="sack"]<-rep(c(1,.5),length.out=sum(d$severity_outcome=="sack"))
d$blocker_ids<-vapply(seq_len(n),function(i)as.character(jsonlite::toJSON(as.character(sample(201:216,if(d$double_team[i])2 else 1)),auto_unbox=FALSE)),character(1))
d$blocker_names<-vapply(d$blocker_ids,function(s)as.character(jsonlite::toJSON(paste0("Blocker ",jsonlite::fromJSON(s)),auto_unbox=FALSE)),character(1))
d$blocker_rated_ol<-vapply(d$blocker_ids,function(s)as.character(jsonlite::toJSON(rep(1,length(jsonlite::fromJSON(s))))),character(1))
d$early_model_eligible[1:5]<-0L;d$win_target[1:5]<-NA;d$severity_outcome[1:10]<-NA
path<-tempfile(fileext=".csv");fwrite(d,path)
x<-read_matchups(path);v<-player_vocabulary(x);cfg<-model_config(lambda_min=.01,lambda_max=1,lambda_length=6);cfg$severity_weights<-c(loss=0,win=.01,pressure=.08,sack=1);cfg$conditional_sack_share<-sack_share(x)
check(nrow(x)==n,"Read changed row count")
check(nrow(model_sample(x,"win"))==n-5L,"Early model must use only observed early grades")
check(nrow(model_sample(x,"severity"))==n-10L,"Final model must use observed early and final grades")
ungraded<-x;ungraded$severity_outcome[is.na(ungraded$win_target)]<-"sack"
check(nrow(model_sample(ungraded,"severity"))==n-10L,"Higher final events cannot bypass early-grade eligibility")
bad_path<-tempfile(fileext=".csv");bad_input<-d;bad_input$severity_outcome[1]<-"pressure";fwrite(bad_input,bad_path)
expect_error(read_matchups(bad_path));unlink(bad_path)
recovered<-x;idx<-which(!is.na(recovered$severity_outcome))[1];recovered$early_model_eligible[idx]<-0L
check(nrow(model_sample(recovered,"win"))==n-6L,"Recovered final row entered original binary sample")
check(nrow(model_sample(recovered,"severity"))==n-10L,"Recovered final row was wrongly excluded from severity")
bad<-d;bad$severity_outcome[11]<-"hit";fwrite(bad,path);expect_error(read_matchups(path))
fwrite(d,path)
m<-matchup_matrix(x,v)
check(max(abs(rowSums(m[,startsWith(colnames(m),"rusher::")])-1))<1e-12,"Rusher mass")
check(max(abs(rowSums(m[,startsWith(colnames(m),"blocker::")])+1))<1e-12,"Blocker mass")
check(all(observation_weights(x,"severity")==1),"Shared sacks must not alter likelihood")
# Group member order and input row order cannot change designs, reference predictions or metrics.
y<-x;y$blocker_keys<-I(lapply(y$blocker_keys,rev));check(max(abs(m-matchup_matrix(y,v)))==0,"Group member order changed design")
folds<-game_folds(x,cfg);duplicated_x<-x[c(seq_len(n),which(x$game_id=="2")),]
check(length(unique(folds$fold[match(duplicated_x$game_id[duplicated_x$game_id=="2"],folds$game_id)]))==1,"Duplicate game fold assignment")
for(model in c("win","severity")) {
  fit<-fit_model(x,model,v,folds,cfg)
  check(fit$centering_max_error<1e-7,"Centering")
  cf<-fit$coefficients
  for(role in c("rusher","blocker"))check(max(abs(colMeans(cf[startsWith(rownames(cf),paste0(role,"::")),,drop=FALSE])))<1e-9,"Role centering")
  if(model=="severity")check(max(abs(rowSums(cf)))<1e-8,"Class centering")
  ref<-reference_matchups(x,model)
  check(abs(sum(ref$Rusher$weight)-1)<1e-10 && abs(sum(ref$Blocker$weight)-1)<1e-10,"Reference mass")
  scores<-player_scores(fit,ref,x,config=cfg)
  check(all(is.finite(scores$score)),"Nonfinite scores")
  check(nrow(scores)==nrow(v$labels)*if(model=="win")2 else 4,"Missing score summaries")
  z<-model_sample(x,model);pred<-predict_model(fit,z);ii<-sample(seq_len(nrow(z)))
  check(abs(log_loss(pred,z,model,cfg)-log_loss(if(model=="win")pred[ii]else pred[ii,,drop=FALSE],z[ii,],model,cfg))<1e-12,"Order dependent loss")
  baseline<-predict_baselines(x,z,model,cfg);check(all(is.finite(unlist(baseline))),"Invalid baseline")
  bs<-baseline_scores(x,model,v,cfg);check(all(is.finite(bs$score)),"Invalid raw/shrunken baseline")
  absent<-x[x$rusher_key!=v$rusher[1],]
  scores_absent<-player_scores(fit,ref,x,absent,cfg)
  check(all(!scores_absent$present_in_draw[scores_absent$role=="Rusher"&scores_absent$player_key==v$rusher[1]]),"Absent player lost")
  if(model=="severity") {
    cfg_na<-cfg;cfg_na$severity_weights["sack"]<-NA_real_
    undefined<-player_scores(fit,ref,x,config=cfg_na)
    check(all(is.na(undefined$score)),"Undefined normalization not preserved")
  }
}
# Intact resampling includes zero-row source games and is schedule independent.
pool<-data.frame(game_id=as.character(1:21),week=c(unique(x[c("game_id","week")])$week,18))
stream<-bootstrap_stream(219L,"ratings",6L);assign(".Random.seed",stream,.GlobalEnv);a<-resample_games(x,pool)
assign(".Random.seed",bootstrap_stream(219L,"ratings",12L),.GlobalEnv);invisible(resample_games(x,pool))
assign(".Random.seed",stream,.GlobalEnv);b<-resample_games(x,pool)
check(identical(a$game_draws,b$game_draws)&&identical(a$data,b$data),"Scheduling changed game sample")
for(g in pool$game_id)check(sum(a$data$game_id==g)==sum(x$game_id==g)*sum(a$game_draws==g),"Partial-game resampling")
expect_error(parse_blockers('["1","1"]'))
expect_error(parse_blockers('[]'))
cat("First-contest matrix, fitting, shared-credit, baselines, reference, absent-player, and resampling tests passed.\n")
