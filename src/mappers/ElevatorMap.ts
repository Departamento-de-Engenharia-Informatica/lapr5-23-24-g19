import { Mapper } from '../core/infra/Mapper'
import Elevator from '../domain/elevator/Elevator'
import { IElevatorDTO } from '../dto/IElevatorDTO'

export class ElevatorMap extends Mapper<Elevator> {
    static toDTO(elevator: Elevator): IElevatorDTO {
        return {
            buildingId: elevator.building.code.value,
            floors: elevator.floors.map(f => f.floorNumber.value),
            identifier: elevator.identifier.value,
            model: elevator.model?.value,
            brand: elevator.brand?.value,
            serialNumber: elevator.serialNumber?.value,
            description: elevator.description?.value,
        }
    }
}
