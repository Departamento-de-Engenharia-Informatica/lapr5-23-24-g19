import { Repo } from '../../core/infra/Repo'
import { Role } from '../../domain/role'
import { RoleId } from '../../domain/roleId'

export default interface IRoleRepo extends Repo<Role> {
    save(role: Role): Promise<Role>
    find(name: string): Promise<Role>
    findByDomainId(roleId: RoleId | string): Promise<Role>
    activeRoles(): Promise<Role[]>

    //findByIds (rolesIds: RoleId[]): Promise<Role[]>;
    //saveCollection (roles: Role[]): Promise<Role[]>;
    //removeByRoleIds (roles: RoleId[]): Promise<any>
}
