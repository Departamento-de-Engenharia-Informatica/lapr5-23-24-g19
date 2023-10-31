FROM node:alpine as base

WORKDIR /be
COPY package.json package-lock.json config.js ./
COPY src ./

RUN npm install

CMD ["npm", "run", "start"]
