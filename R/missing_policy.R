# One row mask is applied before fitting or defining any reference distribution.
.spades_missing_tokens <- function(tokens=character()) {
 if(!is.character(tokens)||anyNA(tokens)||any(!nzchar(trimws(tokens))))
   stop('missing_tokens must be a character vector of nonempty, non-NA labels.')
 unique(c('NA','__MISSING__',trimws(tokens)))
}
.spades_missing_rows <- function(data,tokens) {
 bad<-rep(FALSE,nrow(data))
 for(v in names(data)) {
   z<-data[[v]];bad<-bad|is.na(z)
   if(is.character(z)||is.factor(z))bad<-bad|trimws(as.character(z))%in%tokens
 }
 bad
}
.spades_assert_complete <- function(data,tokens=character(),context='Input') {
 # Plain data-frame callers have no prepared missing-policy metadata.
 if(is.null(tokens))tokens<-character()
 if(!is.data.frame(data)||!nrow(data))stop(context,': nonempty data.frame required.')
 if(any(.spades_missing_rows(data,.spades_missing_tokens(tokens))))
   stop(context,': missing values or missing labels found. Run spades_prepare on the raw input first; downstream rows must not be filtered separately.')
 invisible(TRUE)
}
.spades_check_prepared <- function(prepared) {
 tokens<-prepared$missing_policy$tokens
 if(is.null(tokens))tokens<-character()
 .spades_assert_complete(prepared$data,tokens,'Prepared categorical reference')
 .spades_assert_complete(prepared$original,tokens,'Prepared original reference')
 if(nrow(prepared$data)!=nrow(prepared$original)||
    !identical(names(prepared$data),names(prepared$original)))stop('Prepared original/categorical row alignment mismatch.')
 invisible(TRUE)
}
