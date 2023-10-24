import { Result } from '../../core/logic/Result'
import { ICreatedElevatorDTO } from '../../dto/ICreatedElevatorDTO'
import { IElevatorDTO } from '../../dto/IElevatorDTO'

export default interface IElevatorService {
    createElevator(elevatorDto: IElevatorDTO): Promise<Result<ICreatedElevatorDTO>>
    editElevator(identifier: number, elevatorDto: IElevatorDTO): Promise<Result<ICreatedElevatorDTO>>
    getElevators(code: string): Promise<Result<ICreatedElevatorDTO[]>>
}
