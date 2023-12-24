import { IAuthUserDTO } from '../../dto/IAuthUserDTO'

export default interface IAuthRepo {
    createUser(user: IAuthUserDTO): Promise<String>
}
