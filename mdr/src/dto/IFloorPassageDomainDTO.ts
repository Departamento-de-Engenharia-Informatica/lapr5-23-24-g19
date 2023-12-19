import { Floor } from '../domain/floor/floor'

export interface IFloorPassageDomainDTO {
    from: Floor
    destinations: Floor[]
}
