import { IFilterDTO } from '../../dto/IFilterDTO'
import { IRobotTasksDTO } from '../../dto/IRobotTasksDTO'
import { IRobotTaskSequenceDTO } from '../../dto/IRobotTaskSequenceDTO'
import { ISequenceAlgorithmDTO } from '../../dto/ISequenceAlgorithmDTO'
import { ITaskDTO } from '../../dto/ITaskDTO'
import { IUpdateTaskDTO } from '../../dto/IUpdateTaskDTO'

export default interface IMdtAdapter {
    createSurveillanceTask(dto: ITaskDTO): Promise<String>
    getByFilter(dto: IFilterDTO): Promise<String>
    getByStatus(status: string): Promise<string[]>
    createDeliveryTask(dto: ITaskDTO): Promise<String>
    updateTask(dto: IUpdateTaskDTO): Promise<string>

    taskSequence(dto: IRobotTasksDTO): Promise<IRobotTaskSequenceDTO>
    getTaskSequenceAlgorithms(): Promise<ISequenceAlgorithmDTO[]>
}
