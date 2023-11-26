FROM swipl

WORKDIR /app

COPY . ./

EXPOSE 2228

ENV MDR_PREFIX  "http://localhost:4000/api"

CMD ["swipl", "./main.pl", "-g", "main", "--port=2228", "--fork=false"]
