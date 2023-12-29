import { Service, Inject } from 'typedi'
import config from '../../config'
import ICreateRoleDTO from '../dto/ICreateRoleDTO'
import { Role } from '../domain/role'
import IRoleRepo from '../services/IRepos/IRoleRepo'
import IRoleService from './IServices/IRoleService'
import { Result } from '../core/logic/Result'
import { RoleMap } from '../mappers/RoleMap'
import IUpdateRoleDTO from '../dto/IUpdateRoleDTO'

@Service()
export default class RoleService implements IRoleService {
    constructor(@Inject(config.repos.role.name) private roleRepo: IRoleRepo) {}

    public async getRole(roleId: string): Promise<Result<ICreateRoleDTO>> {
        try {
            const role = await this.roleRepo.findByDomainId(roleId)

            if (role === null) {
                return Result.fail<ICreateRoleDTO>('Role not found')
            } else {
                const roleDTOResult = RoleMap.toDTO(role) as ICreateRoleDTO
                return Result.ok<ICreateRoleDTO>(roleDTOResult)
            }
        } catch (e) {
            throw e
        }
    }

    async getRoles(): Promise<Result<ICreateRoleDTO[]>> {
        const roles = await this.roleRepo.activeRoles()

        return Result.ok(roles.map((r) => RoleMap.toDTO(r)))
    }

    public async createRole(roleDTO: ICreateRoleDTO): Promise<Result<ICreateRoleDTO>> {
        try {
            if (!!(await this.roleRepo.find(roleDTO.name))) {
                return Result.fail({ message: 'Role already exists' })
            }

            const roleOrError = Role.create({ name: roleDTO.name, active: true })

            if (roleOrError.isFailure) {
                return Result.fail<ICreateRoleDTO>(roleOrError.errorValue())
            }

            const roleResult = roleOrError.getValue()

            await this.roleRepo.save(roleResult)

            const roleDTOResult = RoleMap.toDTO(roleResult)
            return Result.ok<ICreateRoleDTO>(roleDTOResult)
        } catch (e) {
            throw e
        }
    }

    public async updateRole(dto: IUpdateRoleDTO): Promise<Result<ICreateRoleDTO>> {
        try {
            const role = await this.roleRepo.find(dto.name)

            if (role === null) {
                return Result.fail<ICreateRoleDTO>('Role not found')
            } else {
                if (dto.active !== undefined) {
                    role.active = dto.active
                }
                await this.roleRepo.save(role)

                const roleDTOResult = RoleMap.toDTO(role)
                return Result.ok<ICreateRoleDTO>(roleDTOResult)
            }
        } catch (e) {
            throw e
        }
    }
}
