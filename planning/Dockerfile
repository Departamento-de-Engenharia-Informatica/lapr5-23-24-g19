FROM swipl

WORKDIR /app

COPY . ./

EXPOSE 2228

CMD ["swipl", "./main.pl", "-g", "main", "--port=2228", "--fork=false"]
