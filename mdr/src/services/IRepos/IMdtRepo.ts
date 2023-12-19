import { ITaskDTO } from '../../dto/ITaskDTO'

export default interface IMdtAdapter {
    createSurveillanceTask(dto: ITaskDTO): Promise<String>
}
