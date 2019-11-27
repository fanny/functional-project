import React, { FC } from 'react'
import Card from '@material-ui/core/Card'
import CardContent from '@material-ui/core/CardContent'
import Typography from '@material-ui/core/Typography'
import { makeStyles } from '@material-ui/core/styles'
import NumAbbr from 'number-abbreviate'

type Props = {
  metric: string,
  value: number,
  avg?: number,
  min?: number,
  max?: number
}

const useStyles = makeStyles({
    card: {
      width: 295,
      margin: '10px',
      display: 'inline-block',
      minHeight: 185,
      justifySelf: 'center',
      alignSelf: 'center',
      textAlign: 'center'
    },
    bullet: {
      display: 'inline-block',
      margin: '0 2px',
      transform: 'scale(0.8)',
    },
    title: {
      fontSize: 14,
    },
    pos: {
      marginBottom: 12,
    },
});


const SummarizingCard:FC<Props> = ({ 
  metric, 
  value, 
  avg, 
  min,
  max
 }) => {
    const classes = useStyles({})
    const numAbbr = new NumAbbr()

    return (
      <Card className={classes.card}>
        <CardContent>
          <Typography className={classes.title} color="textSecondary">
            {metric}
          </Typography>
          <Typography className={classes.pos} variant='h3'>
            {numAbbr.abbreviate(value,2)}
          </Typography>
          {avg && (
            <Typography variant="subtitle2"  color="textSecondary">
              {`Avg: ${numAbbr.abbreviate(avg, 2)}`}
            </Typography>
          )}
          {min && (
            <Typography variant="subtitle2" color="textSecondary">
              {`Min: ${numAbbr.abbreviate(min, 2)}`}
            </Typography>
          )}
          {max && (
            <Typography variant="subtitle2" color="textSecondary">
              {`Max: ${numAbbr.abbreviate(max,2)}`}
            </Typography> 
          )}
        </CardContent>
      </Card>
    )
}

export default SummarizingCard