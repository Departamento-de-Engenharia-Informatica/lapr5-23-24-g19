import { Mapper } from '../core/infra/Mapper'
import { FloorMap } from './FloorMap'

import { IFloorPassageDomainDTO } from '../dto/IFloorPassageDomainDTO'
import { IFloorPassageDTO } from '../dto/IFloorPassageDTO'

export class FloorPassageMap extends Mapper<IFloorPassageDomainDTO> {
    public static toDTO(obj: IFloorPassageDomainDTO): IFloorPassageDTO {
        return {
            floor: FloorMap.toDTO(obj.from),
            passages: obj.destinations.map((p) => FloorMap.toDTO(p)),
        }
    }
}
