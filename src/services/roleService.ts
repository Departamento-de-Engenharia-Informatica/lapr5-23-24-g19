import { Service, Inject } from 'typedi';
import config from "../../config";
import IRoleDTO from '../dto/IRoleDTO';
import { Role } from "../domain/role";
import IRoleRepo from '../services/IRepos/IRoleRepo';
import IRoleService from './IServices/IRoleService';
import { Result } from "../core/logic/Result";
import { RoleMap } from "../mappers/RoleMap";

@Service()
export default class RoleService implements IRoleService {
  constructor(
      @Inject(config.repos.role.name) private roleRepo : IRoleRepo
  ) {}

  public async getRole( roleId: string): Promise<Result<IRoleDTO>> {
    try {
      const role = await this.roleRepo.findByDomainId(roleId);

      if (role === null) {
        return Result.fail<IRoleDTO>("Role not found");
      }
      else {
        const roleDTOResult = RoleMap.toDTO( role ) as IRoleDTO;
        return Result.ok<IRoleDTO>( roleDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }


  public async createRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>> {
    try {

      const roleOrError = await Role.create( roleDTO );

      if (roleOrError.isFailure) {
        return Result.fail<IRoleDTO>(roleOrError.errorValue());
      }

      const roleResult = roleOrError.getValue();

      await this.roleRepo.save(roleResult);

      const roleDTOResult = RoleMap.toDTO( roleResult ) as IRoleDTO;
      return Result.ok<IRoleDTO>( roleDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updateRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>> {
    try {
      const role = await this.roleRepo.findByDomainId(roleDTO.id);

      if (role === null) {
        return Result.fail<IRoleDTO>("Role not found");
      }
      else {
        role.name = roleDTO.name;
        await this.roleRepo.save(role);

        const roleDTOResult = RoleMap.toDTO( role ) as IRoleDTO;
        return Result.ok<IRoleDTO>( roleDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

}
