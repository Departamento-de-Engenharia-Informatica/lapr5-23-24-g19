import { Result } from '../../core/logic/Result'
import { IElevatorDTO } from '../../dto/IElevatorDTO'

export default interface IElevatorService {
    createElevator(elevatorDto: IElevatorDTO): Promise<Result<IElevatorDTO>>
    editElevator(identifier: string, dto: IElevatorDTO): Promise<Result<IElevatorDTO>>
}
