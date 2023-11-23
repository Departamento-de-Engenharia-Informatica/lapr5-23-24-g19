FROM node:alpine AS build

WORKDIR /app

COPY package*.json ./
COPY config.ts ./
COPY src/ ./src

RUN npm install
RUN npm run build

###

FROM nginx:alpine AS prod

WORKDIR /app

COPY --from=build /usr/share/nginx/html
