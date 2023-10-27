import { Repo } from '../../core/infra/Repo'
import { Passage } from '../../domain/passage/passage'
import { BuildingCode } from '../../domain/building/buildingCode'
import { Floor } from '../../domain/floor/floor'
import { IFloorPassageDomainDTO } from '../../dto/IFloorPassageDomainDTO'
import Building from '../../domain/building/building'

export default interface IPassageRepo extends Repo<Passage> {
    find(floor1: Floor, floor2: Floor): Promise<Passage>
    save(passage: Passage): Promise<Passage>
    exists(passage: Passage | string): Promise<boolean>
    findAll(): Promise<Passage[]>
    // FIXME: scuffed due to ObjId's; Building should've been the argument
    passagesBetweenBuildings(dto: Passage[], codex: BuildingCode, codey: BuildingCode): Promise<Passage[]>
    // FIXME: scuffed due to ObjId's; Building should've been the argument
    floorsWithPassage(building: Building): Promise<IFloorPassageDomainDTO[]>
}
