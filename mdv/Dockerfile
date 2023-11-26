FROM node:alpine AS build

WORKDIR /app

COPY package*.json ./
COPY config.ts ./
COPY tsconfig.json ./

COPY src/ ./src
COPY clips/ ./clips
COPY cursors/ ./cursors
COPY models/ ./models
COPY textures/ ./textures
COPY index.html/ ./

COPY .env/ ./
COPY .env.production/ ./

RUN npm install
RUN npm run build

###

FROM nginx:alpine AS prod

EXPOSE 8080

COPY --from=build /app/dist/ /usr/share/nginx/html
COPY --from=build /app/index.html /usr/share/nginx/html
