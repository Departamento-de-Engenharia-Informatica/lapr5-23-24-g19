import GUI from 'lil-gui'
import Dispatcher from './dispatcher'

export default class PassageMenu extends GUI {
    constructor(
        b1: { building: string; floor: number },
        b2: { building: string; floor: number; x: number; y: number },
    ) {
        super({ autoPlace: true })

        const fontSize = '1.5vmin'

        this.title(
            `Passage from &mdash; ${b1.building}${b1.floor} to ${b2.building}${b2.floor}`,
        )
        this.domElement.style.position = 'absolute'
        this.domElement.style.left = '0.5vw'
        this.domElement.style.top = '1.0vh'
        this.domElement.style.fontSize = fontSize

        const buttonFieldName = 'Go to passage'
        this.add(
            {
                [buttonFieldName]: () => {
                    this.hide()
                    this.destroy()

                    const pos = [b2.x, b2.y]
                    Dispatcher.emit('change-map', b2.building, b2.floor, pos)
                },
            },
            buttonFieldName,
        )
    }
}
