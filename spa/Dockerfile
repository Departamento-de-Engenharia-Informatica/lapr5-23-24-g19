FROM node:16 AS build

WORKDIR /app

COPY package*.json ./
COPY tsconfig*.json ./
COPY tailwind.config.js ./
COPY src/ ./src
COPY angular.json ./

RUN npm install -g @angular/cli

RUN npm install
RUN npm run build_prod
#RUN npm run start

##

FROM nginx:alpine AS prod

WORKDIR /app

COPY --from=build dist/spa/* /usr/share/nginx/html/

EXPOSE 80

CMD ["nginx","-g","daemon off;"]
