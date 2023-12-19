import { Mapper } from '../core/infra/Mapper'

import Building from '../domain/building/building'
import { IBuildingFloorNumberDTO } from '../dto/IBuildingFloorNumberDTO'

export class BuildingFloorNumberMap extends Mapper<Building> {
    public static toDTO(
        building: Building,
        floorNumber: number,
    ): IBuildingFloorNumberDTO {
        const { length, width } = building.maxFloorDimensions
        return {
            code: building.code.value,
            name: building.name?.value,
            description: building.description?.value,
            maxFloorDimensions: { length, width },
            floorNumber,
        }
    }
}
