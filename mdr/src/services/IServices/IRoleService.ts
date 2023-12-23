import { Result } from '../../core/logic/Result'
import ICreateRoleDTO from '../../dto/ICreateRoleDTO'
import IUpdateRoleDTO from '../../dto/IUpdateRoleDTO'

export default interface IRoleService {
    createRole(roleDTO: ICreateRoleDTO): Promise<Result<ICreateRoleDTO>>
    updateRole(roleDTO: IUpdateRoleDTO): Promise<Result<ICreateRoleDTO>>
    getRoles(): Promise<Result<ICreateRoleDTO[]>>

    getRole(roleId: string): Promise<Result<ICreateRoleDTO>>
}
