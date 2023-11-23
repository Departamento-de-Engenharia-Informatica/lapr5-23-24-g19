FROM node:alpine AS build

WORKDIR /app

COPY package*.json ./
COPY tsconfig.json ./
COPY config.js ./
COPY src/ ./src

RUN npm install
RUN npm run build

###

FROM node:alpine AS prod

WORKDIR /app

COPY --from=build /app/package*.json ./
COPY --from=build /app/tsconfig.json ./
COPY --from=build /app/build ./build

RUN npm install --only=prod

CMD ["node", "build/src/app.js"]
