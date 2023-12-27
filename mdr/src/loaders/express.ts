import express from 'express'
import bodyParser from 'body-parser'
import cors from 'cors'
import routes from '../api'
import config from '../../config'
import { auth } from 'express-openid-connect'

export default ({ app }: { app: express.Application }) => {
    // Useful if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)
    // It shows the real origin IP in the heroku or Cloudwatch logs
    app.enable('trust proxy')

    // The magic package that prevents frontend developers going nuts
    // Alternate description:
    // Enable Cross Origin Resource Sharing to all origins by default
    app.use(cors())
    /**
     * Health Check endpoints
     * @TODO Explain why they are here
     */
    app.get('/status', (req, res) => {
        res.status(200).end()
    })
    app.head('/status', (req, res) => {
        res.status(200).end()
    })

    // Useful if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)
    // It shows the real origin IP in the heroku or Cloudwatch logs
    app.enable('trust proxy')

    // Some sauce that always add since 2014
    // "Lets you use HTTP verbs such as PUT or DELETE in places where the client doesn't support it."
    // Maybe not needed anymore ?
    app.use(require('method-override')())

    // Middleware that transforms the raw string of req.body into json
    app.use(bodyParser.json())

    // const authconf = {
    //     authRequired: false,
    //     auth0Logout: true,
    //     secret: "BKSxLlyXyUdMaBJ7x5W4Xn7N6Cd30UhHBj6xp55f9GVyMOEzyCqfbPGlSIh1rVEf",
    //     baseURL: 'http://localhost:4000',
    //     clientID: 'VWCGyPRVo5EZ2vlA4T657WddIn0nLVwl',
    //     issuerBaseURL: 'https://dev-wt48psyid1ra2e8l.us.auth0.com'
    // };

    // // auth router attaches /login, /logout, and /callback routes to the baseURL
    // app.use(auth(authconf));

    // // req.isAuthenticated is provided from the auth router
    // app.get('/', (req, res) => {
    //     res.send(req.oidc.isAuthenticated() ? 'Logged in' : 'Logged out');
    // });
    // Load API routes
    app.use(config.api.prefix, routes())

    /// catch 404 and forward to error handler
    app.use((req, res, next) => {
        const err = new Error('Not Found')
        err['status'] = 404
        next(err)
    })

    /// error handlers
    app.use((err, req, res, next) => {
        /**
         * Handle 401 thrown by express-jwt library
         */
        if (err.name === 'UnauthorizedError') {
            return res.status(err.status).send({ message: err.message }).end()
        }
        return next(err)
    })
    app.use((err, req, res, next) => {
        res.status(err.status || 500)
        res.json({
            errors: {
                message: err.message,
            },
        })
    })
}
