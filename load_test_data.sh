DATADIR=./data

myshuf() {
    perl -MList::Util=shuffle -e 'print shuffle(<>);' "$@";
}

normalize_text() {
    tr '[:upper:]' '[:lower:]' | sed -e 's/^/__label__/g' | \
    sed -e "s/'/ ' /g" -e 's/"//g' -e 's/\./ \. /g' -e 's/<br \/>/ /g' \
        -e 's/,/ , /g' -e 's/(/ ( /g' -e 's/)/ ) /g' -e 's/\!/ \! /g' \
        -e 's/\?/ \? /g' -e 's/\;/ /g' -e 's/\:/ /g' | tr -s " " | myshuf
}

mkdir -p "${DATADIR}"

if [ ! -f "${DATADIR}/dbpedia.train" ]
then
  wget -c "https://github.com/le-scientifique/torchDatasets/raw/master/dbpedia_csv.tar.gz" -O "${DATADIR}/dbpedia_csv.tar.gz"
  tar -xzvf "${DATADIR}/dbpedia_csv.tar.gz" -C "${DATADIR}"
  cat "${DATADIR}/dbpedia_csv/train.csv" | normalize_text > "${DATADIR}/dbpedia.train"
  cat "${DATADIR}/dbpedia_csv/test.csv" | normalize_text > "${DATADIR}/dbpedia.test"
fi

if [ ! -f "${DATADIR}/text9" ]
then
    wget -c http://mattmahoney.net/dc/enwik9.zip -P "${DATADIR}"
    unzip "${DATADIR}/enwik9.zip" -d "${DATADIR}"
    perl wikifil.pl "${DATADIR}/enwik9" > "${DATADIR}"/text9
fi

if [ ! -f "${DATADIR}/rw/rw.txt" ]
then
    wget -c http://www-nlp.stanford.edu/~lmthang/morphoNLM/rw.zip -P "${DATADIR}"
    unzip "${DATADIR}/rw.zip" -d "${DATADIR}"
fi