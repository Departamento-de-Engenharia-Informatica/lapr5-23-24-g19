FROM swipl

WORKDIR /app

COPY . ./

ENV PLAN_PORT 2228
# ENV MDR_API  value

EXPOSE 2228

CMD ["swipl", "./main.pl"]
