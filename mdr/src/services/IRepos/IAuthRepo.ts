import { IAssingRoleDTO } from '../../dto/IAssignRoleDTO'
import { IAuthUserDTO } from '../../dto/IAuthUserDTO'

export default interface IAuthRepo {
    createUser(user: IAuthUserDTO): Promise<String>
    assignRoleToUser(dto: IAssingRoleDTO): Promise<void>
}
