curl 'https://query1.finance.yahoo.com/v8/finance/chart/AAPL?region=US&lang=en-US&includePrePost=false&interval=2m&useYfid=true&range=1d&corsDomain=finance.yahoo.com&.tsrc=finance' -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:87.0) Gecko/20100101 Firefox/87.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: https://finance.yahoo.com/quote/AAPL?p=AAPL' -H 'Origin: https://finance.yahoo.com' -H 'Connection: keep-alive' -H 'Cookie: B=6qq6i99g6gibp&b=3&s=7j; GUC=AQABBAFgaZZhVEIdmQRp; A3=d=AQABBHxJaGACENKiMx_X8XkebRZlS9Eg4PsFEgABBAGWaWBUYedVb2UB9iMAAAcIeUloYErSaG0&S=AQAAAjzPMG9VFvVOINQpAqCO4Bw; A1=d=AQABBHxJaGACENKiMx_X8XkebRZlS9Eg4PsFEgABBAGWaWBUYedVb2UB9iMAAAcIeUloYErSaG0&S=AQAAAjzPMG9VFvVOINQpAqCO4Bw; EuConsent=CPEEt7YPEEt7YAOACBITBACoAP_AAH_AACiQHCNd_X_fb39j-_59__t0eY1f9_7_v20zjgeds-8Nyd_X_L8X_2M7vB36pr4KuR4ku3bBAQFtHOncTQmx6IlVqTPsak2Mr7NKJ7PEinsbe2dYGHtfn9VT-ZKZr97s___7________79______3_vt_9__wOCAJMNS-AizEscCSaNKoUQIQriQ6AEAFFCMLRNYQErgp2VwEfoIGACA1ARgRAgxBRiyCAAAAAJKIgBADwQCIAiAQAAgBUgIQAEaAILACQMAgAFANCwAigCECQgyOCo5TAgIkWignkrAEou9jDCEMooAaAGGgAwABA4QRABgACBwgqADAAEDhA; A1S=d=AQABBHxJaGACENKiMx_X8XkebRZlS9Eg4PsFEgABBAGWaWBUYedVb2UB9iMAAAcIeUloYErSaG0&S=AQAAAjzPMG9VFvVOINQpAqCO4Bw&j=GDPR; PRF=t%3DAAPL' -H 'TE: Trailers'


curl 'https://query1.finance.yahoo.com/v8/finance/chart/AAPL?region=US&lang=en-US&includePrePost=false&interval=1d&useYfid=true&range=1d&corsDomain=finance.yahoo.com&.tsrc=finance'

https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1617321600&period2=1617408000&interval=1d&events=history&includeAdjustedClose=true


curl 'https://query1.finance.yahoo.com/v7/finance/download/AAPL?interval=1d&events=history&includeAdjustedClose=true'



curl 'https://query1.finance.yahoo.com/v7/finance/download/AAPL?interval=1d&events=history&includeAdjustedClose=true'
Output example:
Date,Open,High,Low,Close,Adj Close,Volume
2021-04-01,123.660004,124.180000,122.489998,123.000000,123.000000,74957400
                                             ^^^^
                                                                                          