import GUI from 'lil-gui'
import Dispatcher from './dispatcher'

export default class ElevatorMenu extends GUI {
    constructor(
        building: string,
        currentFloor: number,
        floors: number[],
        player?: { position: number[]; direction: number },
    ) {
        super({ autoPlace: true })

        const fontSize = '1.5vmin'

        this.title(`Elevator &mdash; ${building}${currentFloor}`)
        this.domElement.style.position = 'absolute'
        this.domElement.style.left = '0.5vw'
        this.domElement.style.top = '1.0vh'
        this.domElement.style.fontSize = fontSize

        const floorOpt = this.add({ Floor: [] }, 'Floor', floors)

        const buttonFieldName = 'Go to floor'
        this.add(
            {
                [buttonFieldName]: () => {
                    const floor: number = floorOpt.getValue()
                    this.hide()
                    this.destroy()

                    Dispatcher.emit(
                        'change-map',
                        building,
                        floor,
                        player?.position,
                        player?.direction,
                    )
                },
            },
            buttonFieldName,
        )
    }
}
