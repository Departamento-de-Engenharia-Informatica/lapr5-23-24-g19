import { FloorAndBuildingDTO } from '../services/floor.service'

export interface FloorPassageDTO {
    floor: FloorAndBuildingDTO
    passages: FloorAndBuildingDTO[]
}
