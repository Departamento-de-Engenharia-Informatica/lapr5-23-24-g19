import { IAssingRoleDTO } from '../../dto/IAssignRoleDTO'
import { IAuthUserDTO } from '../../dto/IAuthUserDTO'

export default interface IAuthRepo {
    createUser(user: IAuthUserDTO): Promise<String>
    getUserByEmail(email: string): Promise<IAuthUserDTO>
    blockUser(email: string): Promise<void>
    unblockUser(email: string): Promise<void>
    deleteUser(email: string): Promise<void>
    assignRoleToUser(dto: IAssingRoleDTO): Promise<void>
}
