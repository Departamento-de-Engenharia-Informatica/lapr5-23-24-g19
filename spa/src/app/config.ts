import { isDevMode } from '@angular/core'
import { environment } from 'src/environment/environment'
import { environment as prod } from 'src/environment/environment.prod'

export class Config {
    public static baseUrl: string
    public static mdrUrl: string
    public static visualizationUrl: string

    static {
        if (isDevMode()) {
            Config.baseUrl = `${environment.mdrServerUrl}/api`
            Config.mdrUrl = environment.mdrServerUrl
            Config.visualizationUrl = environment.mdvUrl
        } else {
            Config.baseUrl = `${prod.mdrServerUrl}/api`
            Config.mdrUrl = prod.mdrServerUrl
            Config.visualizationUrl = prod.mdvUrl
        }
    }
}
