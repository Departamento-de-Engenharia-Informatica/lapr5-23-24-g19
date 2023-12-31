import { Router } from 'express'
import { AuthRequest, customJwtMiddleware } from '../middlewares/isAuth'

const route = Router()

import { getMe } from '../../controllers/userController'

export default (app: Router) => {
    app.use('/auth', route)

    route.get('/me', customJwtMiddleware, (req, res, next) =>
        getMe(<AuthRequest>req, res, next),
    )
}
