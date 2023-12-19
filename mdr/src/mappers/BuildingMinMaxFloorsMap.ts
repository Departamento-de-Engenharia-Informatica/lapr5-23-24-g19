import { Mapper } from '../core/infra/Mapper'

import Building from '../domain/building/building'
import { IBuildingMinMaxFloorsDTO } from '../dto/IBuildingMinMaxFloorsDTO'

export class BuildingMinMaxFloors extends Mapper<Building> {
    public static toDTO(min: number, max: number): IBuildingMinMaxFloorsDTO {
        return {
            minMaxFloors: { min, max },
        }
    }
}
