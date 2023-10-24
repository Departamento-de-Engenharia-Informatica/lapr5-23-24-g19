import { Repo } from '../../core/infra/Repo'
import { Passage } from '../../domain/passage/passage'
import {BuildingCode} from "../../domain/building/buildingCode";
import {Floor} from "../../domain/floor/floor";
import {IPassageDTO} from "../../dto/IPassageDTO";

export default interface IPassageRepo extends Repo<Passage> {
    save(passage: Passage): Promise<Passage>
    exists(passage: Passage | string): Promise<boolean>
    findAll(): Promise<Passage[]>
    passagesBetweenBuildings(dto: IPassageDTO[], codex: BuildingCode, codey: BuildingCode): Promise<Passage[]>
}
