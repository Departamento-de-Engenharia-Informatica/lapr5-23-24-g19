import { Result } from "../../core/logic/Result";
import IRoleDTO from "../../dto/IRoleDTO";

export default interface IRoleService  {
  createRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>>;
  updateRole(roleDTO: IRoleDTO): Promise<Result<IRoleDTO>>;

  getRole (roleId: string): Promise<Result<IRoleDTO>>;
}
