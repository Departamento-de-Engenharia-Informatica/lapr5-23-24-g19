import { Result } from '../../core/logic/Result'
import { ICreatedElevatorDTO } from '../../dto/ICreatedElevatorDTO'
import { IElevatorDTO } from '../../dto/IElevatorDTO'

export default interface IElevatorService {
    createElevator(elevatorDto: IElevatorDTO): Promise<Result<ICreatedElevatorDTO>>

    // FIXME: use ICreatedElevatorDTO @jonasantunes
    editElevator(identifier: string, dto: IElevatorDTO): Promise<Result<IElevatorDTO>>
    getElevators(code: string): Promise<Result<IElevatorDTO[]>>
}
