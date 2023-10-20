import { Building } from '../domain/building/building'

export interface IFloorBuildingDTO {
    building: Building
    floorNumber: number
    description?: string
}
